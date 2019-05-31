module Backend.Errors where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Data.Variant (Variant, inj)
import Database.PostgreSQL as PostgreSQL
import Prim.Row as R
import Type.Prelude (class IsSymbol, SProxy(..))

class MonadError (Variant e) m <= MonadErrorV (e ∷ # Type) (m ∷ Type → Type)

instance monadErrorV ∷ MonadError (Variant e) m ⇒ MonadErrorV e m

_validation = SProxy ∷ SProxy "validation"

_notFound = SProxy ∷ SProxy "notFound"

_session = SProxy ∷ SProxy "session"

_registration = SProxy ∷ SProxy "registration"

throwV ∷ 
  ∀ e' err sym m a e
  . MonadThrow (Variant e) m
  ⇒ R.Cons sym err e' e
  ⇒ IsSymbol sym
  ⇒ SProxy sym
  → err
  → m a
throwV sym = throwError <<< inj sym

type PGError e = ( pgError ∷ PostgreSQL.PGError | e )

type Registration e = (registration ∷ String | e)

type Validation e = (validation ∷ String | e)

type Session e = (session ∷ String | e)

type NotFound e = (notFound ∷ String | e)
