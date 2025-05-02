{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Kampong.Core.Object
  ( -- * Object Types
    Object(..)
  , ObjectType(..)
  , ObjectAttachment(..)
    
    -- * Object Functions
  , createObject
  , getObject
  , updateObject
  , deleteObject
  ) where

import Protolude
import Kampong.Core.Types (ActorId(..), ObjectId(..))
import Kampong.Core.Object.Types (Object(..), ObjectType(..), ObjectAttachment(..))
import Kampong.Storage.Types (StorageError(..))
import Kampong.Storage.Interface (StorageInterface(..))

-- | Create a new Object
createObject :: StorageInterface m a Object act c -> ObjectId -> ObjectType -> Text -> m (Either StorageError Object)
createObject storage id typ content = do
  let object = Object
        { objectId = id
        , objectType = typ
        , objectContent = content
        , objectActor = ActorId "system"  -- Using a default system actor ID
        , objectAttachments = []
        , objectTo = []
        , objectCc = []
        , objectBto = []
        , objectBcc = []
        , objectAudience = []
        , objectPublished = Nothing
        , objectUpdated = Nothing
        , objectInReplyTo = Nothing
        , objectUrl = Nothing
        , objectTag = []
        , objectReplies = Nothing
        , objectSensitive = False
        , objectSummary = Nothing
        }
  storageCreateObject storage object

-- | Get an Object by ID
getObject :: StorageInterface m a Object act c -> ObjectId -> m (Either StorageError (Maybe Object))
getObject = storageGetObject

-- | Update an existing Object
updateObject :: StorageInterface m a Object act c -> Object -> m (Either StorageError Object)
updateObject = storageUpdateObject

-- | Delete an Object
deleteObject :: StorageInterface m a Object act c -> ObjectId -> m (Either StorageError Bool)
deleteObject = storageDeleteObject 