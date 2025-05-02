{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Kampong.Core.Collection
  ( -- * Collection Types
    Collection(..)
  , CollectionType(..)
  , CollectionPage(..)
    
    -- * Collection Functions
  , createCollection
  , getCollection
  , updateCollection
  , deleteCollection
  , addToCollection
  , removeFromCollection
  ) where

import Protolude
import Prelude (last)
import Kampong.Core.Types (CollectionId(..), URI(..))
import Kampong.Storage.Types (StorageError(..))
import Kampong.Storage.Interface (StorageInterface(..))
import Kampong.Core.Collection.Types (Collection(..), CollectionType(..), CollectionPage(..))

-- | Create a new Collection
createCollection
  :: Monad m
  => CollectionId
  -> CollectionType
  -> m Collection
createCollection id typ = pure $ CollectionData
  { collectionId = id
  , collectionType = typ
  , collectionTotalItems = 0
  , collectionFirst = Nothing
  , collectionLast = Nothing
  , collectionCurrent = Nothing
  , collectionItems = []
  }

-- | Get a Collection by ID
getCollection :: StorageInterface m a o act Collection -> CollectionId -> m (Either StorageError (Maybe Collection))
getCollection = storageGetCollection

-- | Update an existing Collection
updateCollection :: StorageInterface m a o act Collection -> Collection -> m (Either StorageError Collection)
updateCollection = storageUpdateCollection

-- | Delete a Collection
deleteCollection :: StorageInterface m a o act Collection -> CollectionId -> m (Either StorageError Bool)
deleteCollection = storageDeleteCollection

-- | Add an item to a Collection
addToCollection :: MonadIO m => StorageInterface m a o act Collection -> CollectionId -> URI -> m (Either StorageError Collection)
addToCollection storage cid item = do
  maybeCollection <- getCollection storage cid
  case maybeCollection of
    Left err -> pure $ Left err
    Right Nothing -> pure $ Left $ StorageError "Collection not found"
    Right (Just collection) -> do
      let updatedCollection = collection
            { collectionItems = item : collectionItems collection
            , collectionTotalItems = collectionTotalItems collection + 1
            , collectionFirst = case collectionFirst collection of
                Nothing -> Just item
                Just _ -> collectionFirst collection
            , collectionLast = Just item
            }
      updateCollection storage updatedCollection

-- | Remove an item from a Collection
removeFromCollection :: MonadIO m => StorageInterface m a o act Collection -> CollectionId -> URI -> m (Either StorageError Collection)
removeFromCollection storage cid item = do
  maybeCollection <- getCollection storage cid
  case maybeCollection of
    Left err -> pure $ Left err
    Right Nothing -> pure $ Left $ StorageError "Collection not found"
    Right (Just collection) -> do
      let updatedItems = filter (/= item) $ collectionItems collection
          updatedCollection = collection
            { collectionItems = updatedItems
            , collectionTotalItems = length updatedItems
            , collectionFirst = case updatedItems of
                [] -> Nothing
                (x:_) -> Just x
            , collectionLast = case updatedItems of
                [] -> Nothing
                xs -> Just $ Prelude.last xs
            }
      updateCollection storage updatedCollection 