{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Kampong.Core.Types
  ( -- * Core Types
    ActorId(..)
  , ObjectId(..)
  , ActivityId(..)
  , CollectionId(..)
  , URI(..)
  ) where

import Protolude
import Data.Aeson (FromJSON, ToJSON)

-- | Actor ID type
newtype ActorId = ActorId { unActorId :: Text }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Object ID type
newtype ObjectId = ObjectId { unObjectId :: Text }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Activity ID type
newtype ActivityId = ActivityId { unActivityId :: Text }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Collection ID type
newtype CollectionId = CollectionId { unCollectionId :: Text }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | URI type
newtype URI = URI { unURI :: Text }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON) 