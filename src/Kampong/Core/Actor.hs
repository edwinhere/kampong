{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Kampong.Core.Actor
  ( -- * Actor Types
    Actor(..)
  , ActorType(..)
    
    -- * Actor Functions
  , createActor
  , getActor
  , updateActor
  , deleteActor
  , getActorInbox
  , getActorOutbox
  , getActorFollowers
  , getActorFollowing
  , getActorLiked
  ) where

import Protolude
import Kampong.Core.Types (ActorId(..), URI(..))
import Kampong.Storage.Types (StorageError(..))
import Kampong.Storage.Interface (StorageInterface(..))
import Kampong.Core.Actor.Types (Actor(..), ActorType(..), ActorEndpoints(..), endpointInbox, endpointOutbox, endpointFollowing, endpointFollowers, endpointLiked)

-- | Helper function to get an actor's endpoint
getActorEndpoint
  :: MonadIO m
  => StorageInterface m Actor o act a
  -> ActorId
  -> (ActorEndpoints -> Maybe URI)
  -> m (Either StorageError (Maybe URI))
getActorEndpoint storage id endpointGetter = do
  maybeActor <- getActor storage id
  case maybeActor of
    Left err -> pure $ Left err
    Right Nothing -> pure $ Left $ StorageError "Actor not found"
    Right (Just actor) -> pure $ Right $ endpointGetter $ actorEndpoints actor

-- | Create a new Actor
createActor
  :: Monad m
  => ActorId
  -> ActorType
  -> Text
  -> m Actor
createActor id typ name = pure $ Actor
  { actorId = id
  , actorType = typ
  , actorName = name
  , actorPreferredUsername = name
  , actorEndpoints = ActorEndpoints
      { endpointInbox = URI ""
      , endpointOutbox = URI ""
      , endpointFollowing = Nothing
      , endpointFollowers = Nothing
      , endpointLiked = Nothing
      , endpointStreams = []
      }
  , actorPublicKey = Nothing
  , actorIcon = Nothing
  , actorImage = Nothing
  , actorSummary = Nothing
  , actorManuallyApprovesFollowers = False
  , actorDiscoverable = True
  }

-- | Get an Actor by ID
getActor :: StorageInterface m Actor o act a -> ActorId -> m (Either StorageError (Maybe Actor))
getActor = storageGetActor

-- | Update an existing Actor
updateActor :: StorageInterface m Actor o act a -> Actor -> m (Either StorageError Actor)
updateActor = storageUpdateActor

-- | Delete an Actor
deleteActor :: StorageInterface m Actor o act a -> ActorId -> m (Either StorageError Bool)
deleteActor = storageDeleteActor

-- | Get an Actor's inbox
getActorInbox :: MonadIO m => StorageInterface m Actor o act a -> ActorId -> m (Either StorageError (Maybe URI))
getActorInbox storage id = getActorEndpoint storage id (Just . endpointInbox)

-- | Get an Actor's outbox
getActorOutbox :: MonadIO m => StorageInterface m Actor o act a -> ActorId -> m (Either StorageError (Maybe URI))
getActorOutbox storage id = getActorEndpoint storage id (Just . endpointOutbox)

-- | Get an Actor's followers
getActorFollowers :: MonadIO m => StorageInterface m Actor o act a -> ActorId -> m (Either StorageError (Maybe URI))
getActorFollowers storage id = getActorEndpoint storage id endpointFollowers

-- | Get an Actor's following
getActorFollowing :: MonadIO m => StorageInterface m Actor o act a -> ActorId -> m (Either StorageError (Maybe URI))
getActorFollowing storage id = getActorEndpoint storage id endpointFollowing

-- | Get an Actor's liked
getActorLiked :: MonadIO m => StorageInterface m Actor o act a -> ActorId -> m (Either StorageError (Maybe URI))
getActorLiked storage id = getActorEndpoint storage id endpointLiked 