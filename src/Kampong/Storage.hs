{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module Kampong.Storage
  ( -- * Storage Types
    Storage(..)
  , StorageConfig(..)
    
    -- * Storage Functions
  , createStorage
  , getStorage
  , updateStorage
  , deleteStorage
  , createStorageInterface
  ) where

import Protolude
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Kampong.Core.Actor.Types (Actor(..))
import Kampong.Core.Object.Types (Object(..))
import Kampong.Core.Activity.Types (Activity(..))
import Kampong.Core.Collection.Types (Collection(..))
import Kampong.Storage.Interface (StorageInterface(..))
import qualified Kampong.Storage.Types as StorageTypes

-- | Create a new Storage
createStorage :: MonadIO m => m Storage
createStorage = do
  actors <- liftIO $ newIORef []
  objects <- liftIO $ newIORef []
  activities <- liftIO $ newIORef []
  collections <- liftIO $ newIORef []
  pure $ Storage actors objects activities collections

-- | Get a Storage
getStorage :: MonadIO m => Storage -> m Storage
getStorage = pure

-- | Update a Storage
updateStorage :: MonadIO m => Storage -> m Storage
updateStorage = pure

-- | Delete a Storage
deleteStorage :: MonadIO m => Storage -> m Bool
deleteStorage _ = pure True

-- | Storage configuration
data StorageConfig = StorageConfig
  { storageConfigPath :: FilePath
  , storageConfigMaxSize :: Int
  } deriving stock (Generic, Show, Eq)

-- | Storage implementation
data Storage = Storage
  { storageActors :: IORef [Actor]
  , storageObjects :: IORef [Object]
  , storageActivities :: IORef [Activity]
  , storageCollections :: IORef [Collection]
  } deriving stock (Generic)

-- | Create a storage interface from a Storage
createStorageInterface :: Storage -> StorageInterface IO Actor Object Activity Collection
createStorageInterface storage = StorageInterface
  { storageGetActor = \id -> do
      actors <- readIORef $ storageActors storage
      pure $ Right $ find (\a -> actorId a == id) actors

  , storageCreateActor = \actor -> do
      actors <- readIORef $ storageActors storage
      if any (\a -> actorId a == actorId actor) actors
        then pure $ Left $ StorageTypes.StorageError "Actor already exists"
        else do
          writeIORef (storageActors storage) (actor : actors)
          pure $ Right actor

  , storageUpdateActor = \actor -> do
      actors <- readIORef $ storageActors storage
      let updatedActors = map (\a -> if actorId a == actorId actor then actor else a) actors
      writeIORef (storageActors storage) updatedActors
      pure $ Right actor

  , storageDeleteActor = \id -> do
      actors <- readIORef $ storageActors storage
      let updatedActors = filter (\a -> actorId a /= id) actors
      writeIORef (storageActors storage) updatedActors
      pure $ Right True

  , storageGetObject = \id -> do
      objects <- readIORef $ storageObjects storage
      pure $ Right $ find (\o -> objectId o == id) objects

  , storageCreateObject = \obj -> do
      objects <- readIORef $ storageObjects storage
      writeIORef (storageObjects storage) (obj : objects)
      pure $ Right obj

  , storageUpdateObject = \obj -> do
      objects <- readIORef $ storageObjects storage
      let updatedObjects = map (\o -> if objectId o == objectId obj then obj else o) objects
      writeIORef (storageObjects storage) updatedObjects
      pure $ Right obj

  , storageDeleteObject = \id -> do
      objects <- readIORef $ storageObjects storage
      let updatedObjects = filter (\o -> objectId o /= id) objects
      writeIORef (storageObjects storage) updatedObjects
      pure $ Right True

  , storageGetActivity = \id -> do
      activities <- readIORef $ storageActivities storage
      pure $ Right $ find (\a -> activityId a == id) activities

  , storageCreateActivity = \activity -> do
      activities <- readIORef $ storageActivities storage
      writeIORef (storageActivities storage) (activity : activities)
      pure $ Right activity

  , storageUpdateActivity = \activity -> do
      activities <- readIORef $ storageActivities storage
      let updatedActivities = map (\a -> if activityId a == activityId activity then activity else a) activities
      writeIORef (storageActivities storage) updatedActivities
      pure $ Right activity

  , storageDeleteActivity = \id -> do
      activities <- readIORef $ storageActivities storage
      let updatedActivities = filter (\a -> activityId a /= id) activities
      writeIORef (storageActivities storage) updatedActivities
      pure $ Right True

  , storageGetCollection = \id -> do
      collections <- readIORef $ storageCollections storage
      pure $ Right $ find (\c -> collectionId c == id) collections

  , storageCreateCollection = \collection -> do
      collections <- readIORef $ storageCollections storage
      writeIORef (storageCollections storage) (collection : collections)
      pure $ Right collection

  , storageUpdateCollection = \collection -> do
      collections <- readIORef $ storageCollections storage
      let updatedCollections = map (\c -> if collectionId c == collectionId collection then collection else c) collections
      writeIORef (storageCollections storage) updatedCollections
      pure $ Right collection

  , storageDeleteCollection = \id -> do
      collections <- readIORef $ storageCollections storage
      let updatedCollections = filter (\c -> collectionId c /= id) collections
      writeIORef (storageCollections storage) updatedCollections
      pure $ Right True
  } 