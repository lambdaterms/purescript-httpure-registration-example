module Backend.DB.Migrations where

import Prelude

import Backend.Config (parse) as Config
import Data.Either (Either(..))
import Database.PostgreSQL (withConnection, withTransaction)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Global.Unsafe (unsafeStringify)
import Selda (runSelda)
import Selda.PG (MonadSelda)

runMigration ∷ ∀ a. MonadSelda a → Aff Unit
runMigration migration = do
  cfg ← Config.parse
  case cfg of
    Right ctx → do
      withConnection ctx.db case _ of
        Right conn → do
          result ← withTransaction conn (runSelda conn migration)
          case join result of
            Right r → log $ "Successful migration: " <> unsafeStringify r
            Left pgError → log $ "DB error: " <> unsafeStringify pgError
        Left pgError → log $ "DB error: " <> unsafeStringify pgError
    Left err → do
      log $ "Configuration loader error: " <> unsafeStringify err
