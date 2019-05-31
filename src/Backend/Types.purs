module Backend.App.Types where

import Prelude

import Backend.Errors (class MonadErrorV, Error)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader.Trans (class MonadReader, ReaderT, runReaderT)
import Crypto (Secret(..))
import Data.Either (Either)
import DataStore (MemoryStore)
import Database.PostgreSQL (Connection) as PostgreSQL
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import HTTPure (Headers)
import HTTPure (Request) as HTTPure
import Selda as Selda

type Session = { id ∷ String }

type Cookies = { sessionId ∷ String }

type Store = MemoryStore Session

type Context =
  { db ∷ PostgreSQL.Connection
  , debug ∷
      { http ∷ Boolean
      , validateDbJson ∷ Boolean
      }
  , request ∷ HTTPure.Request
  , secred ∷ String
  }

type App = ExceptT Error (ReaderT Context Aff)

runApp ∷ ∀ a. Context → App a → Aff (Either Error a)
runApp ctx m = runReaderT (runExceptT m) ctx


type AppMonad_ err (ctx ∷ # Type) = ExceptT err (ReaderT { | ctx } Aff)

type AppMonad = AppMonad_ Error
  ( secret ∷ Secret
  , store ∷ Store
  , conn ∷ PostgreSQL.Connection
  )

type AppMonadSession = AppMonad_ Error
  ( secret ∷ Secret
  , store ∷ Store
  , conn ∷ PostgreSQL.Connection
  , cookies ∷ Cookies
  , session ∷ Session
  , resHeaders ∷ Headers
  )


-- DB Tables

type UserRow = 
  ( id ∷ Int
  , email ∷ String
  , salt ∷ String
  , hashedPassword ∷ String
  )

type User = Record UserRow

users ∷ Selda.Table UserRow
users = Selda.Table { name: "users" }
