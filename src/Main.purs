module Main where

import Prelude

import Backend.App (app)
import Backend.Config as Config
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Global.Unsafe (unsafeStringify)
import HTTPure as HTTPure

main ∷ Effect Unit
main = launchAff_ $ do
  ectx ← Config.parse
  case ectx of
    Right ctx → do
      void $ liftEffect $ HTTPure.serve'
        { port: ctx.port
        , hostname: "0.0.0.0"
        , backlog: Nothing
        }
        (app ctx)
        (log ("Server up localhost:" <> show ctx.port))
    Left errs → do
      log ("CONFIG PARSE ERROR: " <> unsafeStringify errs)
      pure unit
