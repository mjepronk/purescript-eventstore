module EventStore
  ( Event(..)
  , EventData(..)
  , EventStoreConfig
  , EventStoreError
  , ExpectedVersion(..)
  , mkEventFromString
  , mkEventFromJson
  , writeEvent
  , readEvent
  , readEvents
  , readHead
  , EventNumber
  , URI
  ) where

import Prelude

import Data.Argonaut.Core (Json, stringify, caseJson, fromObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Combinators ((.:))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe')
import Data.UUID (UUID, genUUID, parseUUID, toString)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Milkis as M
import Milkis.Impl (FetchImpl)

import EventStore.Headers (AuthenticationMethod, Headers, addAuthHeader, addHeader, statusText)


type EventStoreConfig
  = { streamURL   :: URI
    , fetchImpl   :: FetchImpl
    , contentType :: String
    , auth        :: AuthenticationMethod
    , headers     :: Headers
    }

data EventData
  = JsonData   Json
  | StringData String

instance eventDataShow :: Show EventData where
  show (JsonData x) = "(JsonData " <> stringify x <> ")"
  show (StringData x) = "(StringData \"" <> x <> "\")"

data Event
  = Event
    { eventId     :: UUID
    , eventType   :: String
    , eventData   :: EventData
    }

instance eventShow :: Show Event where
  show (Event x) = show x


-- Event number (also used for version)
type EventNumber = Int

-- Uniform Resource Locator
type URI = String

data EventStoreError
  = ConnectionError String
  | InvalidRequestError String
  | UnauthorizedError
  | WrongExpectedVersionError
  | DeserialisationError String
  | UnexpectedError String

instance eventStoreErrorShow :: Show EventStoreError where
  show (ConnectionError x)       = "Connection error: " <> x
  show (InvalidRequestError x)   = "Invalid request: " <> x
  show (UnauthorizedError)       = "Unauthorized"
  show WrongExpectedVersionError = "WrongExpectedVersion"
  show (DeserialisationError x)  = "DeserialisationError" <> x
  show (UnexpectedError x)       = "Unexpected: " <> x



-- Optimistic concurrency and idempotence
data ExpectedVersion
  = ExpectedAny
  | ExpectedNoStream
  | ExpectedEmptyStream
  | ExpectedStreamExists
  | ExpectedVersion EventNumber


expectedVersionAsInt :: ExpectedVersion -> Int
expectedVersionAsInt ExpectedEmptyStream  = 0
expectedVersionAsInt ExpectedNoStream     = -1
expectedVersionAsInt ExpectedAny          = -2
expectedVersionAsInt ExpectedStreamExists = -4
expectedVersionAsInt (ExpectedVersion x)  = x


mkEvent ::
  Maybe UUID -> -- ^ Event ID
  String ->     -- ^ Event type
  EventData ->  -- ^ Event data
  Effect Event
mkEvent mayEventId eventType eventData = do
  eventId <- maybe' (const $ liftEffect genUUID) pure mayEventId
  pure (Event { eventId, eventType, eventData: eventData })

mkEventFromString ::
  Maybe UUID -> -- ^ Event ID
  String ->     -- ^ Event type
  String ->     -- ^ Event data
  Effect Event
mkEventFromString mayEventId eventType eventData =
  mkEvent mayEventId eventType (StringData eventData)

mkEventFromJson ::
  Maybe UUID -> -- ^ Event ID
  String ->     -- ^ Event type
  Json ->       -- ^ Event data
  Effect Event
mkEventFromJson mayEventId eventType eventData =
  mkEvent mayEventId eventType (JsonData eventData)


writeEvent ::
  EventStoreConfig -> -- ^ Configuration from `EventStore.Browser` or `EventStore.Node`
  ExpectedVersion ->  -- ^ Value of "ES-ExpectedVersion" header, use `expectedAny` if you don't care
  Event ->            -- ^ Event
  Aff (Either EventStoreError Unit)
writeEvent cfg expectedVersion (Event { eventId, eventType, eventData }) = do
  let
    headers =
      cfg.headers
      # addHeader "Content-Type" cfg.contentType
      # addHeader "ES-EventType" eventType
      # addHeader "ES-EventId" (toString eventId)
      # addHeader "ES-ExpectedVersion" (show (expectedVersionAsInt expectedVersion))
      # addAuthHeader cfg.auth
    opts =
      { method: M.postMethod
      , body:
          case eventData of
            JsonData x   -> stringify x
            StringData x -> x
      , headers: headers
      }

  result <- attempt (M.fetch cfg.fetchImpl (M.URL cfg.streamURL) opts)
  case result of
    Left e -> pure (Left (ConnectionError cfg.streamURL))
    Right resp ->
      case M.statusCode resp of
        201 -> pure (Right unit)
        400 ->
          if statusText resp == "Wrong expected EventNumber"
            then pure (Left (WrongExpectedVersionError))
            else pure (Left (InvalidRequestError (statusText resp)))
        401 -> pure (Left UnauthorizedError)
        415 -> pure (Left (InvalidRequestError (statusText resp)))
        _   -> pure (Left (UnexpectedError ("Unexpected HTTP code, got " <> show (M.statusCode resp))))


readEvent ::
  EventStoreConfig ->
  EventNumber ->
  Aff (Either EventStoreError Event)
readEvent cfg eventNumber = do
  let
    headers =
      cfg.headers
      # addHeader "Accept" "application/vnd.eventstore.atom+json"
      # addAuthHeader cfg.auth
    opts =
      { method: M.getMethod
      , headers: headers
      }

  result <- attempt (M.fetch cfg.fetchImpl (M.URL (cfg.streamURL <> "/" <> show eventNumber)) opts)
  case result of
    Left e -> pure (Left (ConnectionError cfg.streamURL))
    Right resp ->
      case M.statusCode resp of
        200 -> do
          body <- M.text resp
          case decodeReadEventResponse body of
            Right event -> pure (Right event)
            Left error  -> pure (Left (DeserialisationError error))
        _ -> pure (Left (UnexpectedError ("Unexpected HTTP code, got " <> show (M.statusCode resp))))


readHead ::
  EventStoreConfig ->
  Aff (Either EventStoreError EventNumber)
readHead cfg = do
  let
    headers =
      cfg.headers
      # addHeader "Accept" "application/vnd.eventstore.atom+json"
      # addAuthHeader cfg.auth
    opts =
      { method: M.getMethod
      , headers: headers
      }

  result <- attempt (M.fetch cfg.fetchImpl (M.URL (cfg.streamURL <> "/head")) opts)
  case result of
    Left e -> pure (Left (ConnectionError cfg.streamURL))
    Right resp ->
      case M.statusCode resp of
        200 -> do
          body <- M.text resp
          case decodeReadHeadResponse body of
            Right event -> pure (Right event)
            Left error  -> pure (Left (DeserialisationError error))
        _ -> pure (Left (UnexpectedError ("Unexpected HTTP code, got " <> show (M.statusCode resp))))


readEvents ::
  EventStoreConfig -> -- ^ Configuration from `EventStore.Browser` or `EventStore.Node`
  Aff (Either EventStoreError (Array EventNumber))
readEvents cfg = do
  let
    headers =
      cfg.headers
      # addHeader "Accept" "application/vnd.eventstore.atom+json"
      # addAuthHeader cfg.auth
    opts =
      { method: M.getMethod
      , headers: headers }

  result <- attempt (M.fetch cfg.fetchImpl (M.URL (cfg.streamURL <> "?embed=rich")) opts)
  case result of
    Left e -> pure (Left (ConnectionError cfg.streamURL))
    Right resp ->
      case M.statusCode resp of
        200 -> do
          body <- M.text resp
          case decodeReadEventsResponse body of
            Right event -> pure (Right event)
            Left error  -> pure (Left (DeserialisationError error))
        _   -> pure (Left (UnexpectedError ("Unexpected HTTP code, got " <> show (M.statusCode resp))))


decodeReadEventResponse :: String -> Either String Event
decodeReadEventResponse body = do
    case jsonParser body of
      Left _ -> Left "Could not parse JSON"
      Right json -> do
        obj <- decodeJson json
        content <- obj .: "content"
        eventId' <- content .: "eventId"
        eventType <- content .: "eventType"
        eventData' <- content .: "data"
        eventData <- caseJson
          (const (Left (expected <> "Unit")))
          (const (Left (expected <> "Boolean")))
          (const (Left (expected <> "Number")))
          (Right <<< StringData)
          (const (Left (expected <> "Array")))
          (Right <<< JsonData <<< fromObject)
          eventData'
        case parseUUID eventId' of
          Just eventId -> Right (Event { eventId, eventType, eventData })
          Nothing -> Left "Could not decode eventId, not a UUID"
  where
    expected = "Expected String or Object, got "


decodeReadHeadResponse :: String -> Either String EventNumber
decodeReadHeadResponse body = do
    case jsonParser body of
      Left _ -> Left "Could not parse JSON"
      Right json -> do
        obj <- decodeJson json
        content <- obj .: "content"
        content .: "eventNumber"


decodeReadEventsResponse :: String -> Either String (Array EventNumber)
decodeReadEventsResponse body = do
  case jsonParser body of
    Left _ -> Left "Could not parse JSON"
    Right json -> do
      obj <- decodeJson json
      entries <- obj .: "entries"
      traverse decodeEntry entries
  where
    decodeEntry :: Json -> Either String EventNumber
    decodeEntry entry = do
      obj <- decodeJson entry
      eventNumber <- obj .: "eventNumber"
      pure eventNumber
