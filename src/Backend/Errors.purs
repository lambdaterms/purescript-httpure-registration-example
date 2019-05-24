module Backend.Errors where

import Prelude

import Backend.DB.Error as DB.Error
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Variant (Variant, inj)
import Type.Prelude (SProxy(..))
import Type.Row (type (+))

type Validation e = (validation ∷ String | e)
type Session e = (session ∷ String | e)

type NotFound e = (notFound ∷ String | e)

type Error = Variant (Validation + Session + NotFound + DB.Error.Error + ())

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


