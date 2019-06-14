module Backend.Effects where

import Prelude

import Crypto (Secret(..))
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
