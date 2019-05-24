module Backend.Views where

import Prelude

import Data.Array (cons) as Array
import Data.String (joinWith) as String
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import HTTPure (Headers, ResponseM, Path)
import HTTPure (ok, response) as HTTPure
import HTTPure.Response (Response) as HTTPure.Response
import Node.Buffer (Buffer)
import Node.FS.Aff (readFile) as FS

notFound ∷ Path → String → Headers → ResponseM
notFound path body headers =
  HTTPure.response 404 $ "NOT FOUND:\n\n" <> body <> "\n\n" <> show headers <> show path

serveFile ∷ ∀ m. MonadAff m ⇒ String → m HTTPure.Response.Response
serveFile fileName
  = liftAff $ (FS.readFile fileName  ∷ Aff Buffer) >>= HTTPure.ok

indexHtml ∷ ∀ m. MonadAff m ⇒ m HTTPure.Response.Response
indexHtml = serveFile "index.html"

bundleJs ∷ ∀ m. MonadAff m ⇒ m  HTTPure.Response.Response
bundleJs = serveFile "dist/bundle.js"

static ∷ ∀ m. MonadAff m ⇒ Array String → m  HTTPure.Response.Response
static subpath =
  serveFile $ String.joinWith "/" (Array.cons "static" subpath)

pimpJpeg ∷ ∀ m. MonadAff m ⇒ m HTTPure.Response.Response
pimpJpeg = serveFile "prototypes/static/pimp.jpeg"
