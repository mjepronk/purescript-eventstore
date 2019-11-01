module EventStore.Node
  ( mkEventStore
  , mkEventStoreJson
  ) where

import Milkis.Impl.Node (nodeFetch)
import EventStore (EventStoreConfig)
import EventStore.Headers (AuthenticationMethod(..), emptyHeaders)

mkEventStore :: String -> String -> EventStoreConfig
mkEventStore url contentType =
  { streamURL: url
  , fetchImpl: nodeFetch
  , contentType: contentType
  , auth: NoAuthentication
  , headers: emptyHeaders
  }

mkEventStoreJson :: String -> EventStoreConfig
mkEventStoreJson url = mkEventStore url "application/json"
