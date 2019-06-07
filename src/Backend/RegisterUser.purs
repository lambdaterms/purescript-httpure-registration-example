module Backend.RegisterUser where

import Prelude

import Backend.App.Types (RSecret, RSession, User, RMailTransporter, users)
import Backend.Errors (_registration, throwV)
import Backend.Errors as E
import Control.Monad.Except (class MonadError, ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask)
import Crypto (hash, randomSalt, sign, unsign)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Variant (SProxy(..), Variant, inj)
import Database.PostgreSQL (Connection)
import Database.PostgreSQL as PG
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import HTTPure (Response)
import HTTPure as HTTPure
import NodeMailer as Mail
import Selda (class MonadSelda, (.==))
import Selda as Selda
import Selda.PG (hoistSeldaWith)
import Type.Row (type (+))

type Email = String
type Password = String

sendConfirmationEmail ∷
  ∀ m r
  . MonadAff m
  ⇒ MonadAsk { | RMailTransporter r } m
  ⇒ String → String → m Unit
sendConfirmationEmail email text = do
  let 
    msg = 
      { from: "noreply@example.com"
      , to: [ email ]
      , cc: []
      , bcc: []
      , subject: "Confirm Registration Email"
      , text
      , attachments: []
      }
  { mailTransporter } ← ask
  liftAff do
    msgInfo ← Mail.sendMail_ msg mailTransporter
    liftEffect $ log $ "Mail at: " <> (show $ Mail.getTestMessageUrl msgInfo)

sendRegisterConfirmation ∷ 
  ∀ m r
  . MonadAsk { | RSecret + RSession + RMailTransporter r } m
  ⇒ MonadAff m
  ⇒ String
  → m Response
sendRegisterConfirmation email = do
  { secret, session: { id: sessionId } } ← ask
  let code = email <> ";" <> sessionId
  msg ← liftEffect do
    signedCode ← sign secret code
    unsignedsignedCode ← runExceptT $ unsign secret signedCode
    log $ "send email to: " <> email
    log $ "code: " <> code
    log $ "signedCode: " <> signedCode
    log $ "unsignedsignedCode: " <> show unsignedsignedCode
    pure signedCode
  let link = "http://localhost:9000/confirm/" <> msg
  sendConfirmationEmail email link
  liftAff $ HTTPure.ok msg

decodeConfirmationLink ∷ 
  ∀ m e r
  . AppMonad m (E.Registration e) (RSecret r)
  ⇒ String
  → m { email ∷ Email, sessionId ∷ String }
decodeConfirmationLink signed = do
  { secret } ← ask
  code ← liftEffect $ runExceptT $ unsign secret signed
  case split (Pattern ";") <$> code of
    Right [ email, sessionId ] → do
      liftEffect $ log $
        "confirm for: " <> email <> "; sessionId: " <> sessionId
      pure { email, sessionId }
    _ → throwV _registration "wrong confirmation link"

nextId ∷ ∀ m. MonadSelda m ⇒ m Int
nextId = do
  (ids ∷ Array { maxId ∷ Int }) ← Selda.query $ 
    Selda.selectFrom_ do
      Selda.aggregate $ Selda.selectFrom users \user → 
        pure { maxId: Selda.max_ user.id }
      $ \r → do
        maxId ← Selda.notNull r.maxId
        pure { maxId }

  pure $ case ids of
    [ { maxId } ] → maxId + 1
    _ → 0

hashPassword ∷ Password → String → Effect String
hashPassword password salt = hash $ password <> salt

guardEmailUnique ∷
  ∀ m e r
  . AppMonad m (E.Registration e) r
  ⇒ String
  → m Unit
guardEmailUnique email = do
  taken ← hoistSelda $ Selda.query $ Selda.selectFrom users \user → do
    Selda.restrict $ user.email .== Selda.lit email
    pure user
  case head taken of
    Just user → throwV _registration "email already taken"
    Nothing → pure unit

registerUser ∷
  ∀ m e r
  . AppMonad m (E.Registration e) r
  ⇒ Email
  → Password
  → m User
registerUser email password = do
  guardEmailUnique email
  salt ← liftEffect $ randomSalt
  hashedPassword ← liftEffect $ hashPassword password salt
  id ← hoistSelda nextId
  l ← hoistSelda $ Selda.insert users [ { id, email, salt, hashedPassword } ]
  case head l of
    Just user → pure user
    Nothing → throwV _registration "could not insert to the db"

-- future selda

class 
  ( MonadAff m
  , MonadError (Variant ( pgError ∷ PG.PGError | e ) ) m
  , MonadReader { conn ∷ PG.Connection | r } m
  ) <= AppMonad m e r

instance appMonadInstance
  ∷ ( MonadAff m
    , MonadError (Variant ( pgError ∷ PG.PGError | e ) ) m
    , MonadReader { conn ∷ PG.Connection | r } m
    )
  ⇒ AppMonad m e r

-- hoistSelda ∷
--   ∀ m e r
--   . MonadAff m
--   ⇒ MonadError (Variant ( DBError.Error e ) ) m
--   ⇒ MonadReader { conn ∷ Connection | r } m
--   ⇒ Selda.PG.MonadSelda ~> m
hoistSelda ∷
  ∀ m e r
  . AppMonad m e r
  ⇒ ExceptT PG.PGError (ReaderT PG.Connection Aff) ~> m
hoistSelda = hoistSeldaWith fe fr
  where
    fe ∷ PG.PGError → Variant ( pgError ∷ PG.PGError | e )
    fe = inj (SProxy ∷ SProxy "pgError")
    fr ∷ { conn ∷ Connection | r } → Connection
    fr { conn } = conn
