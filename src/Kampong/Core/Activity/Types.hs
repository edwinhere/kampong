{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Kampong.Core.Activity.Types
  ( -- * Activity Types
    Activity(..)
  , ActivityType(..)
  , ActivityObject(..)
  ) where

import Protolude
import Data.Aeson (FromJSON, ToJSON)
import Kampong.Core.Types (ActorId(..), ObjectId(..), ActivityId(..), URI(..))
import Kampong.Core.Object.Types (Object(..))

-- | Represents the type of an Activity in the ActivityPub protocol
data ActivityType
  = Create
  | Update
  | Delete
  | Follow
  | Accept
  | Reject
  | Add
  | Remove
  | Like
  | Block
  | Undo
  | Announce
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Represents the object of an Activity
data ActivityObject
  = ObjectRef ObjectId
  | ObjectEmbed Object
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Represents an Activity in the ActivityPub protocol
data Activity = Activity
  { activityId :: ActivityId
  , activityType :: ActivityType
  , activityActor :: ActorId
  , activityObject :: ActivityObject
  , activityTo :: [URI]
  , activityCc :: [URI]
  , activityBto :: [URI]
  , activityBcc :: [URI]
  , activityAudience :: [URI]
  , activityPublished :: Maybe Text
  , activityUpdated :: Maybe Text
  } deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON) 