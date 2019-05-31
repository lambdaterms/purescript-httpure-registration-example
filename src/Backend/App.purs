module Backend.App where

import Prelude

import Backend.App.Types (AppMonad, AppMonadSession, Session)
import Backend.Config (Config, parse) as Config
import Backend.RegisterUser (Email, handleConfirmation, sendRegisterConfirmation)
import Backend.Session (cookiesSessionMiddleware)
import Backend.Views (serveFile)
import Control.Monad.Except (runExcept, runExceptT, throwError)
import Control.Monad.Reader (runReaderT)
import Crypto (Secret(..))
import Data.Array (head, uncons)
import Data.Either (Either(..))
import Data.Map (Map, empty)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replace)
import DataStore (memoryStore)
import Database.PostgreSQL (withConnection)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref (new)
import Global.Unsafe (unsafeStringify)
import HTTPure (Method(..), ResponseM)
import HTTPure as HTTPure
import HTTPure.Utils (urlDecode)

main ∷ Effect Unit
main = launchAff_ $ do
  ectx ← Config.parse
  case ectx of
    Right ctx → do
      void $ liftEffect $ HTTPure.serve'
        { port: ctx.port
        , hostname: "0.0.0.0"
        , backlog: Nothing
        }
        (app ctx)
        (log ("Server up localhost:" <> show ctx.port))
    Left errs → do
      log ("CONFIG PARSE ERROR: " <> unsafeStringify errs)
      pure unit

router ∷ HTTPure.Request → AppMonad ResponseM
router = cookiesSessionMiddleware $ \req@{ method, path, headers, body } → do
  case method, uncons path of
    Post, Just { head: "register" } → do
      liftEffect $ log body
      sendRegisterConfirmation (parseBodyToEmail body)
    Get, Just { head: "confirm", tail } → do
      case head tail of
        Nothing → pure HTTPure.notFound
        Just signedCode → do
          liftEffect $ log signedCode
          handleConfirmation signedCode
          -- pure $ HTTPure.internalServerError "notimplemeneted"
    _, _ →
      pure $ serveFile "register.html"
  where
    parseBodyToEmail ∷ String → Email
    parseBodyToEmail =
      replace (Pattern "email=") (Replacement "") >>> urlDecode

app ∷ Config.Config → HTTPure.Request → Aff HTTPure.Response
app { db: pool, debug, secret } request = do
  ref ← liftEffect $ new (empty ∷ Map String Session)
  withConnection pool case _ of
    Left pgError → do
      -- XXX: Do proper logging
      HTTPure.internalServerError "Pg connection error..."
    Right db → do
      let ctx = { store: memoryStore ref, secret: Secret secret, conn: db }
      runReaderT (runExceptT $ router request) ctx >>= case _ of
        Left err → HTTPure.internalServerError $ show err
        Right res → res

        -- _, Just { head: "api", tail: subpath } →
        --   Views.Api.router (ctx { request { path = subpath }})
        -- method, pathParts → Views.Web.runApp ctx $ case method, pathParts of
        --   Get, Nothing → Views.indexHtml
        --   Get, Just { head: "bundle.js" } → Views.bundleJs
        --   Get, Just { head: "static", tail } → Views.static tail
        --   -- _, Just { head: "test-order", tail } → Store.router $ req { path = tail }
        --   _, _ → liftAff $ Views.notFound path body headers

