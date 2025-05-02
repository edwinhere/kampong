{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Kampong.Core.Object.Types
  ( -- * Object Types
    Object(..)
  , ObjectType(..)
  , ObjectAttachment(..)
  ) where

import Protolude
import Data.Aeson (FromJSON, ToJSON)
import Kampong.Core.Types (ActorId(..), ObjectId(..), URI(..))

-- | Represents the type of an Object in the ActivityPub protocol
data ObjectType
  = Note
  | Article
  | Image
  | Video
  | Audio
  | Document
  | Page
  | Event
  | Place
  | Profile
  | Tombstone
  | Relationship
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Represents an attachment to an Object
data ObjectAttachment = ObjectAttachment
  { attachmentType :: ObjectType
  , attachmentUrl :: URI
  , attachmentName :: Maybe Text
  , attachmentMediaType :: Maybe Text
  , attachmentWidth :: Maybe Int
  , attachmentHeight :: Maybe Int
  , attachmentDuration :: Maybe Int
  } deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Represents an Object in the ActivityPub protocol
data Object = Object
  { objectId :: ObjectId
  , objectType :: ObjectType
  , objectActor :: ActorId
  , objectContent :: Text
  , objectAttachments :: [ObjectAttachment]
  , objectTo :: [URI]
  , objectCc :: [URI]
  , objectBto :: [URI]
  , objectBcc :: [URI]
  , objectAudience :: [URI]
  , objectPublished :: Maybe Text
  , objectUpdated :: Maybe Text
  , objectInReplyTo :: Maybe ObjectId
  , objectUrl :: Maybe URI
  , objectTag :: [URI]
  , objectReplies :: Maybe URI
  , objectSensitive :: Bool
  , objectSummary :: Maybe Text
  } deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON) 