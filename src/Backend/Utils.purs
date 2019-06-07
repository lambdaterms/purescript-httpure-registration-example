module Backend.Utils where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.List.NonEmpty (singleton)
import Data.Maybe (maybe)
import Foreign (ForeignError(..), MultipleErrors, renderForeignError)
import Foreign.Generic (defaultOptions, genericDecodeJSON)
import Foreign.Generic.Class (class GenericDecode)
import Foreign.Generic.Types (Options)

decode ∷
  ∀ a rep
  . Generic a rep
  ⇒ GenericDecode rep
  ⇒ String
  → Either MultipleErrors a
decode s = runExcept $ genericDecodeJSON opts s

parseId ∷ String → Either MultipleErrors Int
parseId s = maybe
  (Left $ singleton $ ForeignError $ "Wrong \"id\" value: " <> s)
  Right $ fromString s

opts ∷ Options
opts = defaultOptions { unwrapSingleConstructors = true }

renderForeignErrors ∷ ∀ f. Foldable f ⇒ Functor f ⇒ f ForeignError → String
renderForeignErrors = joinWith' "\n" <<< map renderForeignError

joinWith' ∷ ∀ f a. Foldable f ⇒ Show a ⇒ String → f a → String
joinWith' sep f = foldl (\a x → a <> show x <> sep) "" f

