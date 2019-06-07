module Backend.App where

import Prelude

import Backend.App.Types (AppMonad_, RConn, RCookies, RResHeaders, RSecret, RSession, RStore, Session, RMailTransporter)
import Backend.Config (Config)
import Backend.Errors as E
import Backend.RegisterUser (class AppMonad, Email, decodeConfirmationLink, registerUser, sendRegisterConfirmation)
import Backend.Session (cookiesSessionMiddleware)
import Backend.Views (serveFile)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Crypto (Secret(..))
import Data.Array (head, uncons)
import Data.Either (Either(..))
import Data.Map (Map, empty)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), joinWith, replace)
import DataStore (memoryStore)
import Database.PostgreSQL (withConnection)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref (new)
import HTTPure (Method(..), Response, Request)
import HTTPure as HTTPure
import HTTPure.Utils (urlDecode)
import NodeMailer as Mail
import Type.Row (type (+))

type Error e = E.Validation + E.Session + E.NotFound + E.PGError + E.Registration e

type App = AppMonad_ ( | Error () )
  (RSecret + RStore + RConn + RMailTransporter ())

type AppSession = AppMonad_ ( Error () )
  (RSecret + RStore + RConn + RCookies + RSession + RResHeaders ())

confirmRoute ∷
  ∀ m e r
  . AppMonad m (E.Registration e) (RSecret r)
  ⇒ Method → String → String → m Response
confirmRoute method body signedCode = do
  log signedCode
  { email, sessionId } ← decodeConfirmationLink signedCode
  case method of
    Get → 
      serveFile "register-password.html"
    Post → do
      let password = fromBody "password" body
      log $ "password: " <> password
      -- TODO: check password, does body contain it?
      user ← registerUser email password
      log $ "registered: " <> (joinWith ", " 
        [show user.id, user.email, user.hashedPassword, user.salt])
      liftAff $ HTTPure.ok "registered"
    _ → liftAff HTTPure.notFound

router ∷ HTTPure.Request → App Response
router = cookiesSessionMiddleware $ \req@{ method, path, headers, body } → do
  case method, uncons path of
    Post, Just { head: "register" } → do
      liftEffect $ log body
      sendRegisterConfirmation (fromBody "email" body)
    _, Just { head: "confirm", tail } → do
      -- path: /confirm/signedCode
      -- GET: show register form
      -- POST: register user; password should be in body
      case head tail of
        Nothing → liftAff HTTPure.notFound
        Just signedCode → do
          confirmRoute method body signedCode
    _, _ →
      serveFile "register.html"

-- works for body that is "<key>=<value>"
fromBody ∷ String → String → Email
fromBody key =
  replace (Pattern $ key <> "=") (Replacement "") >>> urlDecode

app ∷ Config → Request → Aff Response
app { db: pool, debug, secret } request = do
  ref ← liftEffect $ new (empty ∷ Map String Session)
  withConnection pool case _ of
    Left pgError → do
      -- XXX: Do proper logging
      HTTPure.internalServerError "Pg connection error.."
    Right db → do
      mailTransporter ← do 
        config ← Mail.createTestAccount
        liftEffect $ Mail.createTransporter config
      let
        ctx = 
          { store: memoryStore ref
          , secret: Secret secret
          , conn: db
          , mailTransporter
          }
      runReaderT (runExceptT $ router request) ctx >>= case _ of
        Left err → HTTPure.internalServerError $ show err
        Right res → pure res
