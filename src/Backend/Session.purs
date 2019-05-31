module Backend.Session
  ( cookiesSessionMiddleware
  ) where

import Prelude

import Backend.App.Types (AppMonad, AppMonadSession, Session)
import Control.Error.Util (hushT)
import Control.Monad.Except (mapExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader (ask, withReaderT)
import Cookies (Name, Value, Values, CookieAttributes, defaultCookieAttributes, parseCookies, setCookieHeaderValue)
import Crypto (Secret, sign, unsign)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.NonEmpty as NonEmpty
import DataStore (MemoryStore)
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import HTTPure (Headers, Request, ResponseM, header, (!@))
import HTTPure.Headers as Headers
import Record (merge)

cookiesSessionMiddleware
  ∷ (Request → AppMonadSession ResponseM)
  → (Request → AppMonad ResponseM)
cookiesSessionMiddleware router req@{ headers } = do
  -- get session, in case of failure (no valid cookie or sessionId expired) create and Set-Cookie
  { session, resHeaders } ← getSession headers >>= case _ of
    Just session → 
      pure { session, resHeaders: Headers.empty }
    Nothing → do
      { store, secret } ← ask
      session ← liftEffect $ createSession store 
      hv ← liftEffect $ setCookieHeaderSignedValue sessionIdKey session.id defaultCookieAttributes secret
      pure { session, resHeaders: header "Set-Cookie" hv }

  mapExceptT (withReaderT \ctx → merge ctx
    { session
    , cookies: { sessionId: session.id } 
    , resHeaders
    }) $ router req

getSession ∷ Headers → AppMonad (Maybe Session)
getSession headers = do
  { store, secret } ← ask
  liftEffect $ headers !@ "cookie" # parseCookies # hush # toCookies secret
    >>= case _ of
      Nothing → pure Nothing
      Just c → liftEffect $ store.get c.sessionId

toCookies ∷ Secret → Maybe (Object Values) → Effect (Maybe { sessionId ∷ String })
toCookies s obj = runMaybeT do
  signed ← MaybeT $ pure $ obj >>= Object.lookup sessionIdKey <#> NonEmpty.head
  sessionId ← hushT $ unsign s signed
  pure { sessionId }

sessionIdKey ∷ String
sessionIdKey = "sessionId"

createSession ∷ MemoryStore Session → Effect Session
createSession store = do
  id ← store.create
  let session = { id }
  store.set id session
  pure session

setCookieHeaderSignedValue ∷ Name → Value → CookieAttributes → Secret → Effect String
setCookieHeaderSignedValue k v attrs s = do
  signed ← sign s v
  pure $ setCookieHeaderValue k signed attrs
