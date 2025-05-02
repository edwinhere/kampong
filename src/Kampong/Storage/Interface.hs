{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Kampong.Storage.Interface
  ( -- * Storage Interface
    StorageInterface(..)
  ) where

import Protolude
import Kampong.Core.Types (ActorId(..), ObjectId(..), ActivityId(..), CollectionId(..))
import Kampong.Core.Object.Types ()
import Kampong.Storage.Types (StorageError(..))

-- | Storage interface
data StorageInterface m a o act c = StorageInterface
  { storageGetActor :: ActorId -> m (Either StorageError (Maybe a))
  , storageCreateActor :: a -> m (Either StorageError a)
  , storageUpdateActor :: a -> m (Either StorageError a)
  , storageDeleteActor :: ActorId -> m (Either StorageError Bool)
    
  , storageGetObject :: ObjectId -> m (Either StorageError (Maybe o))
  , storageCreateObject :: o -> m (Either StorageError o)
  , storageUpdateObject :: o -> m (Either StorageError o)
  , storageDeleteObject :: ObjectId -> m (Either StorageError Bool)
    
  , storageGetActivity :: ActivityId -> m (Either StorageError (Maybe act))
  , storageCreateActivity :: act -> m (Either StorageError act)
  , storageUpdateActivity :: act -> m (Either StorageError act)
  , storageDeleteActivity :: ActivityId -> m (Either StorageError Bool)
    
  , storageGetCollection :: CollectionId -> m (Either StorageError (Maybe c))
  , storageCreateCollection :: c -> m (Either StorageError c)
  , storageUpdateCollection :: c -> m (Either StorageError c)
  , storageDeleteCollection :: CollectionId -> m (Either StorageError Bool)
  } 