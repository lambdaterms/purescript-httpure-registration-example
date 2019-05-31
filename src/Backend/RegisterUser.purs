module Backend.RegisterUser where

import Prelude

import Backend.App.Types (AppMonadSession, users, User)
import Backend.DB.Error as DBError
import Backend.Errors (class MonadErrorV, Error, throwRegistrationError, throwValidationError)
import Backend.Errors as Errors
import Control.Monad.Except (class MonadError, runExceptT)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ask)
import Crypto (Secret(..), hash, randomSalt, sign, unsign)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), joinWith, split)
import Data.Variant (SProxy(..), Variant)
import Database.PostgreSQL (Connection, PGError)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import HTTPure (ResponseM)
import HTTPure as HTTPure
import HTTPure.Utils (urlDecode)
import Selda ((.==))
import Selda as Selda
import Selda.PG (MonadSelda)
import Type.Row (type (+))

type Email = String
type Password = String

-- newtype RegisterForm = RegisterForm { email ∷ Email }

-- derive instance newtypeRegisterForm ∷ Newtype RegisterForm _
-- derive instance genericRegisterForm ∷ Generic RegisterForm _

-- instance decodeRegisterForm ∷ Decode RegisterForm where
--   decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

-- instance encodeRegisterForm ∷ Encode RegisterForm where
--   encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

sendRegisterConfirmation ∷ 
  ∀ sctx m r
  . Bind m
  ⇒ MonadAsk { secret ∷ Secret, session ∷ { id ∷ String | sctx } | r } m
  ⇒ MonadEffect m
  ⇒ String
  → m ResponseM
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
  -- send email
  pure $ HTTPure.ok msg

-- handleConfirmation ∷ String → AppMonadSession ResponseM
handleConfirmation signed = do
  { secret } ← ask
  code ← liftEffect $ runExceptT $ unsign secret signed
  case split (Pattern ";") <$> code of
    Right [ email, sessionId ] → do
      liftEffect $ log $
        "register: " <> email <> " for sessionId: " <> sessionId
      let password = "pass123"
      registerUser email password >>= case _ of
        Just user → do
          liftEffect $ log $ "registered: " <> (joinWith ", " 
            [show user.id, user.email, user.hashedPassword, user.salt])
          pure $ HTTPure.ok "registered"
        Nothing → throwRegistrationError "could not insert to the db"
    _ → throwValidationError ""

nextId ∷ MonadSelda Int
nextId = do
  (ids ∷ Array { maxId ∷ Int }) ← Selda.query $ Selda.aggregate $
    Selda.selectFrom users \user → pure { maxId: Selda.max_ user.id }
  pure $ case ids of
    [ { maxId } ] → maxId + 1
    _ → 0

hashPassword ∷ Password → String → Effect String
hashPassword password salt = hash $ password <> salt

guardEmailUnique ∷
  ∀ m e r
  . Bind m 
  ⇒ MonadAff m 
  ⇒ MonadErrorV (DBError.Error + Errors.Registration e ) m 
  ⇒ MonadReader { conn ∷ Connection | r } m
  ⇒ String
  → m Unit
guardEmailUnique email = do
  taken ← hoistSelda $ Selda.query $ Selda.selectFrom users \user → do
    Selda.restrict $ user.email .== Selda.lit email
    pure user
  case head taken of
    Just user → throwRegistrationError "email already taken"
    Nothing → pure unit

-- registerUser ∷ Email → Password → MonadSelda (Maybe User)
registerUser email password = do
  guardEmailUnique email
  salt ← liftEffect $ randomSalt
  hashedPassword ← liftEffect $ hashPassword password salt
  id ← hoistSelda nextId
  hoistSelda $ head <$> Selda.insert users [ { id, email, salt, hashedPassword } ]

hoistSelda ∷
  ∀ m e r
  . MonadAff m
  ⇒ MonadError (Variant ( DBError.Error e ) ) m
  ⇒ MonadReader { conn ∷ Connection | r } m
  ⇒ MonadSelda ~> m
hoistSelda = Selda.hoistSeldaWith DBError.db fr
  where
    fr ∷ { conn ∷ Connection | r } → Connection
    fr { conn } = conn
