module Backend.App.Types where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Crypto as Crypto
import Data.Variant (Variant)
import DataStore (MemoryStore)
import Database.PostgreSQL (Connection) as PostgreSQL
import Effect.Aff (Aff)
import HTTPure as HTTPure
import NodeMailer as Mail
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

type AppMonad_ err ctx = ExceptT (Variant err) (ReaderT { | ctx } Aff)

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

-- Reader Contexts

type RSecret r = ( secret ∷ Crypto.Secret | r )

type RStore r = ( store ∷ Store | r )

type RConn r = ( conn ∷ PostgreSQL.Connection | r )

type RCookies r = ( cookies ∷ Cookies | r )

type RSession r = ( session ∷ Session | r )

type RResHeaders r = ( resHeaders ∷ HTTPure.Headers | r )

type RMailTransporter r = ( mailTransporter ∷ Mail.Transporter | r )
