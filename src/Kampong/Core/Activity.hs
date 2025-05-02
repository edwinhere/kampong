{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Kampong.Core.Activity
  ( -- * Activity Types
    Activity(..)
  , ActivityType(..)
  , ActivityObject(..)
    
    -- * Activity Functions
  , createActivity
  , getActivity
  , updateActivity
  , deleteActivity
  , deliverActivity
  ) where

import Protolude
import Data.Aeson (encode)
import Kampong.Core.Types (ActorId(..), ActivityId(..), URI(..))
import Kampong.Core.Activity.Types (Activity(..), ActivityType(..), ActivityObject(..))
import Kampong.Storage.Types (StorageError(..))
import Kampong.Storage.Interface (StorageInterface(..))
import Network.HTTP.Client (newManager, parseRequest, httpLbs, responseStatus, method, requestHeaders, RequestBody(..), requestBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (status200, status201, status202, status204)

-- | Create a new Activity
createActivity
  :: Monad m
  => ActivityId
  -> ActivityType
  -> ActorId
  -> ActivityObject
  -> [URI]
  -> [URI]
  -> m Activity
createActivity id typ actor obj recipients cc = pure Activity
  { activityId = id
  , activityType = typ
  , activityActor = actor
  , activityObject = obj
  , activityTo = recipients
  , activityCc = cc
  , activityBto = []
  , activityBcc = []
  , activityAudience = []
  , activityPublished = Nothing
  , activityUpdated = Nothing
  }

-- | Get an Activity by ID
getActivity :: StorageInterface m a o Activity c -> ActivityId -> m (Either StorageError (Maybe Activity))
getActivity = storageGetActivity

-- | Update an existing Activity
updateActivity :: StorageInterface m a o Activity c -> Activity -> m (Either StorageError Activity)
updateActivity = storageUpdateActivity

-- | Delete an Activity
deleteActivity :: StorageInterface m a o Activity c -> ActivityId -> m (Either StorageError Bool)
deleteActivity = storageDeleteActivity

-- | Deliver an Activity to its recipients
deliverActivity :: MonadIO m => Activity -> m Bool
deliverActivity activity = do
  manager <- liftIO $ newManager tlsManagerSettings
  let recipients = activityTo activity ++ activityCc activity
  results <- forM recipients $ \recipient -> do
    let url = unURI recipient
    request <- liftIO $ parseRequest $ toS url
    let request' = request
          { method = "POST"
          , requestBody = RequestBodyLBS $ encode activity
          , requestHeaders = [("Content-Type", "application/activity+json")]
          }
    response <- liftIO $ httpLbs request' manager
    pure $ responseStatus response `elem` [status200, status201, status202, status204]
  pure $ all identity results 