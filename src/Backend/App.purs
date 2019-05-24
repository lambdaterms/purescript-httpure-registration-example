module Backend.App where

import Prelude

import Backend.App.Types (AppMonad, AppMonadSession, Session)
import Backend.Config (Config, parse) as Config
import Backend.Session (cookiesSessionMiddleware)
import Backend.Views (serveFile)
import Control.Monad.Except (runExcept, runExceptT)
import Control.Monad.Reader (ask, runReaderT)
import Crypto (Secret(..), sign, unsign)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Array (uncons)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Map (Map, empty)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), Replacement(..), replace)
import DataStore (memoryStore)
import Database.PostgreSQL (withConnection)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import Effect.Ref (new)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (decodeJSON, defaultOptions, genericDecode, genericEncode)
import Global.Unsafe (unsafeStringify)
import HTTPure (Method(..), ResponseM)
import HTTPure (Request, Response, badRequest, internalServerError, ok, serve') as HTTPure
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


type Email = String

-- newtype RegisterForm = RegisterForm { email ∷ Email }

-- derive instance newtypeRegisterForm ∷ Newtype RegisterForm _
-- derive instance genericRegisterForm ∷ Generic RegisterForm _

-- instance decodeRegisterForm ∷ Decode RegisterForm where
--   decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

-- instance encodeRegisterForm ∷ Encode RegisterForm where
--   encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


registerAndSendConfirm ∷ Email → AppMonadSession ResponseM
registerAndSendConfirm email = do
  { secret, session: { id: sessionId } } ← ask
  let code = email <> "." <> sessionId
  msg ← liftEffect do
    signedCode ← sign secret code
    unsignedsignedCode ← runExceptT $ unsign secret signedCode
    log $ "send email to: " <> email
    log $ "code: " <> code
    log $ "signedCode: " <> signedCode
    log $ "unsignedsignedCode: " <> show unsignedsignedCode
    pure signedCode
  pure $ HTTPure.ok msg

parseBodyToEmail ∷ String → String
parseBodyToEmail =
  replace (Pattern "email=") (Replacement "") >>> urlDecode

router ∷ HTTPure.Request → AppMonad ResponseM
router = cookiesSessionMiddleware $ \req@{ method, path, headers, body } → do
  case method, uncons path of
    Post, Just { head: "register", tail } → do
      liftEffect $ log body
      registerAndSendConfirm (parseBodyToEmail body)
    _, _ →
      pure $ serveFile "register.html"

app ∷ Config.Config → HTTPure.Request → Aff HTTPure.Response
app { db: pool, debug, secret } request = do
  ref ← liftEffect $ new (empty ∷ Map String Session)
  withConnection pool case _ of
    Left pgError → do
      -- XXX: Do proper logging
      HTTPure.internalServerError "Pg connection error..."
    Right db → do
      let ctx = { store: memoryStore ref, secret: Secret secret }
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

