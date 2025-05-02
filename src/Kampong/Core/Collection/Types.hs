{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Kampong.Core.Collection.Types
  ( -- * Collection Types
    Collection(..)
  , CollectionType(..)
  , CollectionPage(..)
  ) where

import Protolude
import Data.Aeson (FromJSON, ToJSON)
import Kampong.Core.Types (CollectionId(..), URI(..))

-- | Represents the type of a Collection in the ActivityPub protocol
data CollectionType
  = Collection
  | OrderedCollection
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Represents a page of a Collection
data CollectionPage = CollectionPage
  { pageId :: URI
  , pagePartOf :: URI
  , pageNext :: Maybe URI
  , pagePrev :: Maybe URI
  , pageItems :: [URI]
  , pageTotalItems :: Int
  } deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Represents a Collection in the ActivityPub protocol
data Collection = CollectionData
  { collectionId :: CollectionId
  , collectionType :: CollectionType
  , collectionTotalItems :: Int
  , collectionFirst :: Maybe URI
  , collectionLast :: Maybe URI
  , collectionCurrent :: Maybe URI
  , collectionItems :: [URI]
  } deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON) 