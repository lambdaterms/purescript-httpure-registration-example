module Backend.ReaderContexts where

import Backend.App.Types as Types
import Crypto as Crypto
import Database.PostgreSQL as PostgreSQL
import HTTPure.Headers (Headers)

type Secret r = ( secret ∷ Crypto.Secret | r )

type Store r = ( store ∷ Types.Store | r )

type Conn r = ( conn ∷ PostgreSQL.Connection | r )

type Cookies r = ( cookies ∷ Types.Cookies | r )

type Session r = ( session ∷ Types.Session | r )

type ResHeaders r = ( resHeaders ∷ Headers | r )
