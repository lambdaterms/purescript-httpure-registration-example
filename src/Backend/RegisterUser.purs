module Backend.RegisterUser where

import Prelude

import Backend.App.Types (User, users)
import Backend.Errors (throwRegistrationError, throwValidationError)
import Backend.Errors as E
import Backend.ReaderContexts as R
import Control.Monad.Except (class MonadError, runExceptT)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ask)
import Crypto (hash, randomSalt, sign, unsign)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), joinWith, split)
import Data.Variant (SProxy(..), Variant, inj)
import Database.PostgreSQL (Connection)
import Database.PostgreSQL as PG
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import HTTPure (ResponseM)
import HTTPure as HTTPure
import Selda ((.==))
import Selda as Selda
import Selda.PG as Selda.PG
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
  ∀ m r
  . MonadAsk { | R.Secret + R.Session r } m
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

handleConfirmation ∷ 
  ∀ m e r
  . MonadSelda m (E.Registration + E.Validation e) (R.Secret r)
  ⇒ String
  → m ResponseM
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

nextId ∷ Selda.PG.MonadSelda Int
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
  . MonadSelda m (E.Registration e) r
  ⇒ String
  → m Unit
guardEmailUnique email = do
  taken ← hoistSelda $ Selda.query $ Selda.selectFrom users \user → do
    Selda.restrict $ user.email .== Selda.lit email
    pure user
  case head taken of
    Just user → throwRegistrationError "email already taken"
    Nothing → pure unit

registerUser ∷
  ∀ m e r
  . MonadSelda m (E.Registration e) r
  ⇒ Email
  → Password
  → m (Maybe User)
registerUser email password = do
  guardEmailUnique email
  salt ← liftEffect $ randomSalt
  hashedPassword ← liftEffect $ hashPassword password salt
  id ← hoistSelda nextId
  hoistSelda $ head <$> Selda.insert users [ { id, email, salt, hashedPassword } ]


-- future selda

class 
  ( MonadAff m
  , MonadError (Variant ( pgError ∷ PG.PGError | e ) ) m
  , MonadReader { conn ∷ PG.Connection | r } m
  ) <= MonadSelda m e r

instance monadSeldaInstance
  ∷ ( MonadAff m
    , MonadError (Variant ( pgError ∷ PG.PGError | e ) ) m
    , MonadReader { conn ∷ PG.Connection | r } m
    )
  ⇒ MonadSelda m e r

-- hoistSelda ∷
--   ∀ m e r
--   . MonadAff m
--   ⇒ MonadError (Variant ( DBError.Error e ) ) m
--   ⇒ MonadReader { conn ∷ Connection | r } m
--   ⇒ Selda.PG.MonadSelda ~> m
hoistSelda ∷
  ∀ m e r
  . MonadSelda m e r
  ⇒ Selda.PG.MonadSelda ~> m
hoistSelda = Selda.hoistSeldaWith fe fr
  where
    fe ∷ PG.PGError → Variant ( pgError ∷ PG.PGError | e )
    fe = inj (SProxy ∷ SProxy "pgError")
    fr ∷ { conn ∷ Connection | r } → Connection
    fr { conn } = conn