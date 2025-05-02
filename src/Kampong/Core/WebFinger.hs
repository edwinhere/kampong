{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}

module Kampong.Core.WebFinger
  ( -- * WebFinger Types
    WebFingerResource(..)
  , WebFingerLink(..)
  , WebFingerProperty(..)
    
    -- * WebFinger Functions
  , lookupWebFinger
  , createWebFingerResource
  , createActorLink
  , createProfileLink
  , createInboxLink
  , createOutboxLink
  ) where

import Protolude
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import qualified Data.Text as T
import Kampong.Core (URI(..))
import Kampong.Error (KampongError(..), NetworkError(..))
import Kampong.Logging (LogLevel(..), logMessage, Logger)
import Network.HTTP.Client (newManager, parseRequest, httpLbs, responseStatus, responseBody, method, requestHeaders)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (status200)

-- | Represents a WebFinger resource
data WebFingerResource = WebFingerResource
  { resourceSubject :: Text
  , resourceAliases :: [Text]
  , resourceLinks :: [WebFingerLink]
  , resourceProperties :: [WebFingerProperty]
  } deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Represents a WebFinger link
data WebFingerLink = WebFingerLink
  { linkRel :: Text
  , linkType :: Maybe Text
  , linkHref :: URI
  , linkTitles :: [(Text, Text)]
  , linkProperties :: [WebFingerProperty]
  } deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Represents a WebFinger property
data WebFingerProperty = WebFingerProperty
  { propertyName :: Text
  , propertyValue :: Text
  } deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Look up a WebFinger resource
lookupWebFinger
  :: (MonadIO m, MonadReader Logger m)
  => Text
  -> m (Either KampongError WebFingerResource)
lookupWebFinger resource = do
  logMessage Info "WebFinger" "Looking up WebFinger resource" [("resource", resource)]
  manager <- liftIO $ newManager tlsManagerSettings
  
  -- Parse the resource string (e.g., "acct:user@example.com")
  let (_, rest) = T.breakOn ":" resource
      (_, domain) = T.breakOn "@" $ T.drop 1 rest
  
  -- Construct the WebFinger URL
  let webfingerUrl = "https://" <> domain <> "/.well-known/webfinger?resource=" <> resource
  
  -- Make the HTTP request
  request <- liftIO $ parseRequest $ T.unpack webfingerUrl
  let request' = request
        { method = "GET"
        , requestHeaders = [("Accept", "application/jrd+json")]
        }
  
  response <- liftIO $ httpLbs request' manager
  
  -- Check if the request was successful
  if responseStatus response == status200
    then case eitherDecode $ responseBody response of
      Left err -> do
        logMessage Error "WebFinger" "Failed to parse WebFinger response" [("error", toS err)]
        pure $ Left $ NetworkError $ NetworkInvalidResponse $ "Failed to parse WebFinger response: " <> toS err
      Right resource' -> do
        logMessage Info "WebFinger" "Successfully retrieved WebFinger resource" [("resource", resource)]
        pure $ Right resource'
    else do
      logMessage Error "WebFinger" "WebFinger request failed" [("status", show $ responseStatus response)]
      pure $ Left $ NetworkError $ NetworkInvalidResponse $ "WebFinger request failed with status: " <> show (responseStatus response)

-- | Create a new WebFinger resource
createWebFingerResource
  :: Monad m
  => Text
  -> [Text]
  -> [WebFingerLink]
  -> [WebFingerProperty]
  -> m WebFingerResource
createWebFingerResource subject aliases links properties = pure WebFingerResource
  { resourceSubject = subject
  , resourceAliases = aliases
  , resourceLinks = links
  , resourceProperties = properties
  }

-- | Helper function to create an ActivityPub actor link
createActorLink :: URI -> WebFingerLink
createActorLink actorUri = WebFingerLink
  { linkRel = "self"
  , linkType = Just "application/activity+json"
  , linkHref = actorUri
  , linkTitles = []
  , linkProperties = []
  }

-- | Helper function to create a profile link
createProfileLink :: URI -> WebFingerLink
createProfileLink profileUri = WebFingerLink
  { linkRel = "http://webfinger.net/rel/profile-page"
  , linkType = Just "text/html"
  , linkHref = profileUri
  , linkTitles = []
  , linkProperties = []
  }

-- | Helper function to create an inbox link
createInboxLink :: URI -> WebFingerLink
createInboxLink inboxUri = WebFingerLink
  { linkRel = "http://www.w3.org/ns/activitystreams#inbox"
  , linkType = Just "application/activity+json"
  , linkHref = inboxUri
  , linkTitles = []
  , linkProperties = []
  }

-- | Helper function to create an outbox link
createOutboxLink :: URI -> WebFingerLink
createOutboxLink outboxUri = WebFingerLink
  { linkRel = "http://www.w3.org/ns/activitystreams#outbox"
  , linkType = Just "application/activity+json"
  , linkHref = outboxUri
  , linkTitles = []
  , linkProperties = []
  } 