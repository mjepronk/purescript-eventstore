module EventStore.Browser
  ( mkEventStore
  , mkEventStoreJson
  ) where

import Milkis.Impl.Window (windowFetch)
import EventStore (EventStoreConfig)
import EventStore.Headers (AuthenticationMethod(..), emptyHeaders)

mkEventStore :: String -> String -> EventStoreConfig
mkEventStore url contentType =
  { streamURL: url
  , fetchImpl: windowFetch
  , contentType: contentType
  , auth: NoAuthentication
  , headers: emptyHeaders
  }

mkEventStoreJson :: String -> EventStoreConfig
mkEventStoreJson url = mkEventStore url "application/json"
