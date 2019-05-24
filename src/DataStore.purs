module DataStore where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe)
import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

type Key = String

-- | You can always use ceated keys to set values - here is the law:
-- | delete key >>= get key = pure Nothing
type Store m a =
  { create ∷ m Key
  , delete ∷ Key → m Unit
  , get ∷ Key → m (Maybe a)
  , set ∷ Key → a → m Unit
  }

hoist ∷ ∀ a m m'. (m ~> m') → Store m a → Store m' a
hoist h s = { create, delete, get, set }
  where
  create = h s.create
  delete = h <$> s.delete
  get = h <$> s.get
  set k = h <$> s.set k

type MemoryStore a = Store Effect a

memoryStore ∷ ∀ a. Ref (Map.Map String a) → MemoryStore a
memoryStore ref = { create, delete, get, set }
  where
  create = show <$> genUUID
  delete key = void $ Ref.modify (Map.delete key) ref
  get key = Ref.read ref >>= (Map.lookup key >>> pure)
  set key a = void $ Ref.modify (Map.insert key a) ref
