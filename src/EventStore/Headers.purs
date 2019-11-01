module EventStore.Headers
  ( Headers
  , AuthenticationMethod(..)
  , emptyHeaders
  , addHeader
    -- * Internal
  , addAuthHeader
  , statusText
  ) where

import Prelude
import Data.String.Base64 as B64
import Foreign.Object (Object, empty, insert)
import Milkis as M
import Unsafe.Coerce (unsafeCoerce)

type Headers = Object String

data AuthenticationMethod
  = BasicAuth String String -- ^ HTTP Basic Authentication
  | NoAuthentication -- ^ Use your own authentication method, set your headers on the `EventStoreConfig`

emptyHeaders :: Headers
emptyHeaders =
    empty
    # addHeader "User-Agent" ("PureScript EventStore client v" <> version)
  where version = "0.0.1"

addHeader ::
  String ->  -- ^ HTTP header field name
  String ->  -- ^ HTTP header field value
  Headers -> -- ^ Headers to add to, for example `emptyHeaders`
  Headers
addHeader = insert

-- * Internal

addAuthHeader :: AuthenticationMethod -> Headers -> Headers
addAuthHeader (BasicAuth user pass) hs = addHeader "Authorization" header hs
  where header = "Basic " <> (B64.encode (user <> ":" <> pass))
addAuthHeader NoAuthentication hs = hs

statusText :: M.Response -> String
statusText response = response'.statusText
  where
    response' :: { statusText :: String }
    response' = unsafeCoerce response
