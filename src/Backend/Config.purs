module Backend.Config where

import Prelude

-- import Backend.Logging (Logger, defaultConfig, logger) as Logging
import Data.Either (Either(..))
import Database.PostgreSQL (Pool, PoolConfiguration) as PostgreSQL
import Database.PostgreSQL (PGError, newPool)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (MultipleErrors)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Simple.JSON (readJSON)

data Error = ParseError MultipleErrors | DBError PGError

type ConfigJson =
  { db ∷ PostgreSQL.PoolConfiguration
  , debug ∷
      -- show debug output on frontend
      { http ∷ Boolean
      , validateDbJson ∷ Boolean
      }
  , port ∷ Int
  , secret ∷ String
  }

type Config =
  { db ∷ PostgreSQL.Pool
  , debug ∷
      -- show debug output on frontend
      { http ∷ Boolean
      , validateDbJson ∷ Boolean
      }
  , port ∷ Int
  , secret ∷ String
  -- , logger ∷ Logging.Logger
  }

parse ∷ Aff (Either Error Config)
parse = do
  strConfig ← readTextFile ASCII "backend.json"
  case readJSON strConfig of
    Right (config ∷ ConfigJson) → liftEffect $ do
      db ← newPool config.db
      -- logger ← Logging.logger Logging.defaultConfig
      -- pure (Right { db, debug: config.debug, logger, port: config.port })
      pure (Right { db, debug: config.debug, port: config.port, secret: config.secret })
    Left errs → pure (Left $ ParseError errs)

