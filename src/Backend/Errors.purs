module Backend.Errors where

import Prelude

import Backend.DB.Error as DB.Error
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Variant (Variant, inj)
import Type.Prelude (SProxy(..))
import Type.Row (type (+))

class MonadError (Variant e) m <= MonadErrorV (e ∷ # Type) (m ∷ Type → Type)

instance monadErrorV ∷ MonadError (Variant e) m ⇒ MonadErrorV e m

type Validation e = (validation ∷ String | e)
type Session e = (session ∷ String | e)

type NotFound e = (notFound ∷ String | e)

type Error = Variant (Validation + Session + NotFound + DB.Error.Error + Registration + ())

_validation = SProxy ∷ SProxy "validation"

validationError ∷ ∀ e. String → (Variant (Validation + e))
validationError = inj _validation

throwValidationError ∷ ∀ a e m. MonadError (Variant (Validation + e)) m ⇒ String → m a
throwValidationError = throwError <<< validationError

_notFound = SProxy ∷ SProxy "notFound"

throwNotFoundError ∷ ∀ a e m. MonadError (Variant (NotFound + e)) m ⇒ String → m a
throwNotFoundError = throwError <<< inj _notFound

_session = SProxy ∷ SProxy "session"

session ∷ String → Error
session = inj _session


type Registration e = (registration ∷ String | e)

_registration = SProxy ∷ SProxy "registration"

throwRegistrationError ∷ ∀ a e m. MonadError (Variant (Registration + e)) m ⇒ String → m a
throwRegistrationError = throwError <<< inj _registration

