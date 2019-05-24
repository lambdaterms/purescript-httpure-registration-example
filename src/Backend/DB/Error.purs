module Backend.DB.Error where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Variant (Variant, inj) as Variant
import Database.PostgreSQL.PG (PGError)
import Type.Prelude (SProxy(..))
import Type.Row (type (+))

_db = SProxy ∷ SProxy "db"

type Error e = (db ∷ PGError | e)
type Variant e = Variant.Variant (Error + e)

db ∷ ∀ e. PGError → Variant e
db = Variant.inj _db

throw ∷ ∀ a e m. MonadError (Variant e) m ⇒ PGError → m a
throw = throwError <<< db

