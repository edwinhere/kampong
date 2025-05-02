{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Kampong.Core.Actor.Types
  ( -- * Actor Types
    Actor(..)
  , ActorType(..)
  , ActorEndpoints(..)
  ) where

import Protolude
import Data.Aeson (FromJSON, ToJSON)
import Kampong.Core.Types (ActorId(..), URI(..))

-- | Represents the type of an Actor in the ActivityPub protocol
data ActorType
  = Person
  | Application
  | Group
  | Organization
  | Service
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Represents the endpoints available for an Actor
data ActorEndpoints = ActorEndpoints
  { endpointInbox :: URI
  , endpointOutbox :: URI
  , endpointFollowing :: Maybe URI
  , endpointFollowers :: Maybe URI
  , endpointLiked :: Maybe URI
  , endpointStreams :: [URI]
  } deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Represents an Actor in the ActivityPub protocol
data Actor = Actor
  { actorId :: ActorId
  , actorType :: ActorType
  , actorName :: Text
  , actorPreferredUsername :: Text
  , actorEndpoints :: ActorEndpoints
  , actorPublicKey :: Maybe Text
  , actorIcon :: Maybe URI
  , actorImage :: Maybe URI
  , actorSummary :: Maybe Text
  , actorManuallyApprovesFollowers :: Bool
  , actorDiscoverable :: Bool
  } deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON) 