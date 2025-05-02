{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Kampong.Server
  ( -- * Server Types
    ServerConfig(..)
  , Server(..)
    
    -- * Server Functions
  , runServer
  , createServer
  ) where

import Protolude hiding (ByteString)
import Data.Aeson (encode, decode, object, (.=), ToJSON)
import Data.List (lookup)
import Data.ByteString.Lazy (fromStrict, ByteString)
import Network.Wai (Application, Request(..), Response, responseLBS, requestMethod, pathInfo, queryString, getRequestBodyChunk)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status201, status202, status400, status404, status415, status500, methodGet, methodPost, Status)
import Kampong.Core (ActorId(..), ObjectId(..), ActivityId(..), CollectionId(..))
import Kampong.Core.Actor (Actor, getActor)
import Kampong.Core.Activity (Activity, getActivity)
import Kampong.Core.Object (getObject)
import Kampong.Core.Collection (getCollection)
import Kampong.Core.WebFinger (lookupWebFinger)
import Kampong.Storage (Storage, createStorageInterface)
import Kampong.Storage.Interface (StorageInterface(..))
import qualified Kampong.Storage.Types as StorageTypes
import Kampong.Error (KampongError(..), StorageError(..))
import Kampong.Logging (createLogger, LogLevel(..))

-- | Convert StorageError to KampongError
toKampongError :: StorageTypes.StorageError -> KampongError
toKampongError (StorageTypes.StorageError err) = StorageError $ StorageOperationError err

-- | Helper function to create error response
errorResponse :: Status -> Text -> Response
errorResponse status msg = responseLBS status [] $ encode $ object ["error" .= msg]

-- | Helper function to create success response with content type
successResponse :: Status -> Text -> ByteString -> Response
successResponse status contentType = responseLBS status [("Content-Type", encodeUtf8 contentType)]

-- | Helper function to handle storage errors
handleStorageError :: Either StorageTypes.StorageError a -> Either Text a
handleStorageError (Left err) = Left $ case toKampongError err of
  StorageError (StorageOperationError msg) -> msg
  NetworkError msg -> show msg
  ValidationError msg -> show msg
  InternalError msg -> show msg
handleStorageError (Right a) = Right a

-- | Helper function to handle GET requests for resources
handleGetRequest :: (ToJSON b) => (StorageInterface IO a o act c -> id -> IO (Either StorageTypes.StorageError (Maybe b)))
                -> StorageInterface IO a o act c
                -> id
                -> Text
                -> IO Response
handleGetRequest getter storage id' notFoundMsg = do
  result <- getter storage id'
  case handleStorageError result of
    Left err -> pure $ errorResponse status500 err
    Right Nothing -> pure $ errorResponse status404 notFoundMsg
    Right (Just resource) -> pure $ successResponse status200 "application/activity+json" $ encode resource

-- | Server configuration
data ServerConfig = ServerConfig
  { serverHost :: Text
  , serverPort :: Int
  , serverStorage :: Storage
  , serverActor :: Actor
  } deriving stock (Generic)

-- | Server instance
data Server = Server
  { serverConfig :: ServerConfig
  , serverApplication :: Application
  } deriving stock (Generic)

-- | Create a new server
createServer
  :: Monad m
  => ServerConfig
  -> m Server
createServer config = pure Server
  { serverConfig = config
  , serverApplication = application config
  }

-- | Run the server
runServer
  :: MonadIO m
  => ServerConfig
  -> m ()
runServer config = do
  server <- createServer config
  liftIO $ run (serverPort config) (serverApplication server)

-- | Convert KampongError to Text
errorToText :: KampongError -> Text
errorToText (StorageError (StorageOperationError msg)) = msg
errorToText (NetworkError err) = show err
errorToText (ValidationError err) = show err
errorToText (InternalError msg) = msg

-- | The WAI application
application :: ServerConfig -> Application
application config request respond = do
  let storageInterface = createStorageInterface (serverStorage config)
  logger <- liftIO $ createLogger Info
  case (requestMethod request, pathInfo request) of
    -- WebFinger endpoint
    (method, ["well-known", "webfinger"]) | method == methodGet -> do
      case lookup "resource" (queryString request) of
        Just (Just resource) -> do
          resource' <- liftIO $ runReaderT (lookupWebFinger (decodeUtf8 resource)) logger
          case resource' of
            Left err -> respond $ errorResponse status400 $ errorToText err
            Right r -> respond $ successResponse status200 "application/jrd+json" $ encode r
        _ -> respond $ errorResponse status400 "Missing resource parameter"
    
    -- Actor endpoint
    (method, ["actors", actorId]) | method == methodGet ->
      respond =<< handleGetRequest getActor storageInterface (ActorId actorId) "Actor not found"
    
    -- Object endpoint
    (method, ["objects", objectId]) | method == methodGet ->
      respond =<< handleGetRequest getObject storageInterface (ObjectId objectId) "Object not found"
    
    -- Activity endpoint
    (method, ["activities", activityId]) | method == methodGet ->
      respond =<< handleGetRequest getActivity storageInterface (ActivityId activityId) "Activity not found"
    
    -- Collection endpoint
    (method, ["collections", collectionId]) | method == methodGet ->
      respond =<< handleGetRequest getCollection storageInterface (CollectionId collectionId) "Collection not found"
    
    -- Inbox endpoint
    (method, ["inbox"]) | method == methodPost -> do
      body <- getRequestBodyChunk request
      case decode (Data.ByteString.Lazy.fromStrict body) :: Maybe Activity of
        Nothing -> respond $ errorResponse status415 "Invalid ActivityPub payload"
        Just _ -> do
          -- Process the incoming activity
          -- TODO: Implement activity processing
          respond $ successResponse status202 "application/json" $ encode $ object ["status" .= ("Activity accepted" :: Text)]
    
    -- Outbox endpoint
    (method, ["outbox"]) | method == methodPost -> do
      body <- getRequestBodyChunk request
      case decode (Data.ByteString.Lazy.fromStrict body) :: Maybe Activity of
        Nothing -> respond $ errorResponse status415 "Invalid ActivityPub payload"
        Just _ -> do
          -- Process the outgoing activity
          -- TODO: Implement activity delivery
          respond $ successResponse status201 "application/json" $ encode $ object ["status" .= ("Activity created" :: Text)]
    
    -- Default case
    _ -> respond $ errorResponse status404 "Not found" 