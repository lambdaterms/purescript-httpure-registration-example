module Backend.Effects where

import Prelude

import Backend.App.Types (User)
import Crypto (Secret)
import Data.Either (Either)
import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)
import Run (Run)
import Run as Run

-- Mailer Effect

data MailerF a
  = SendMail 
      { from ∷ String
      , to ∷ String
      , subject ∷ String
      , text ∷ String
      }
      (String → a)

derive instance functorMailerF ∷ Functor MailerF

type MAILER = FProxy MailerF

_mailer = SProxy ∷ SProxy "mailer"

sendMail ∷
  ∀ r
  . { from ∷ String
    , to ∷ String
    , subject ∷ String
    , text ∷ String
    }
  → Run (mailer ∷ MAILER | r) String
sendMail msg = Run.lift _mailer (SendMail msg identity)


-- Hmac Effect

data HmacF a 
  = HmacSign Secret String (String → a)
  | HmacUnsign Secret String (Either String String → a)

derive instance functorHmacF ∷ Functor HmacF

type HMAC = FProxy HmacF

_hmac = SProxy ∷ SProxy "hmac"

sign ∷
  ∀ r
  . Secret
  → String
  → Run (hmac ∷ HMAC | r) String
sign secret s = Run.lift _hmac (HmacSign secret s identity)

unsign ∷
  ∀ r
  . Secret
  → String
  → Run (hmac ∷ HMAC | r) (Either String String)
unsign secret s = Run.lift _hmac (HmacUnsign secret s identity)


-- Selda Effect

data SeldaF a
  = FilterUsersByEmail String (Array User → a)
  | InsertUsers (Array User) (Array User → a)
  | MaxUserId (Array { maxId ∷ Int} → a)
  -- | forall r. MaxId (Table ( id ∷ Int | r )) ...

derive instance functorSeldaF ∷ Functor SeldaF

type SELDA = FProxy SeldaF

_selda = SProxy ∷ SProxy "selda"

filterUsersByEmail ∷ ∀ r. String → Run (selda ∷ SELDA | r) (Array User)
filterUsersByEmail email = Run.lift _selda (FilterUsersByEmail email identity)

insertUsers ∷ ∀ r. (Array User) → Run (selda ∷ SELDA | r) (Array User)
insertUsers users = Run.lift _selda (InsertUsers users identity)

maxUserId ∷ ∀ r. Run (selda ∷ SELDA | r) (Array { maxId ∷ Int})
maxUserId = Run.lift _selda (MaxUserId identity)


-- Hash Effect

data HashF a
  = HashIt String (String → a)
  | Salt (String → a)

derive instance functorHashF ∷ Functor HashF

type HASH = FProxy HashF

_hash = SProxy ∷ SProxy "hash"

hash ∷ ∀ r. String → Run (hash ∷ HASH | r) String
hash s = Run.lift _hash (HashIt s identity)

randomSalt ∷ ∀ r. Run (hash ∷ HASH | r) String
randomSalt = Run.lift _hash (Salt identity)
