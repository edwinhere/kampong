{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}

module Kampong.Logging
  ( -- * Logging Types
    LogLevel(..)
  , LogMessage(..)
  , Logger(..)
    
    -- * Logging Functions
  , createLogger
  , logMessage
  , withLogging
  ) where

import Protolude hiding (hPutStrLn)
import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime, getCurrentTime)
import System.IO (hFlush, hPutStrLn)
import qualified Data.Text as T

-- | Log levels
data LogLevel
  = Debug
  | Info
  | Warning
  | Error
  deriving stock (Generic, Show, Eq, Enum, Bounded, Ord)
  deriving anyclass (FromJSON, ToJSON)

-- | A log message
data LogMessage = LogMessage
  { messageTimestamp :: UTCTime
  , messageLevel :: LogLevel
  , messageComponent :: Text
  , messageText :: Text
  , messageMetadata :: [(Text, Text)]
  } deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Logger configuration
data Logger = Logger
  { loggerHandle :: Handle
  , loggerMinLevel :: LogLevel
  , loggerFormat :: LogMessage -> Text
  }

-- | Create a new logger
createLogger :: MonadIO m => LogLevel -> m Logger
createLogger minLevel = do
  pure Logger
    { loggerHandle = stdout
    , loggerMinLevel = minLevel
    , loggerFormat = formatLogMessage
    }

-- | Log a message
logMessage :: (MonadIO m, MonadReader Logger m) => LogLevel -> Text -> Text -> [(Text, Text)] -> m ()
logMessage level component text metadata = do
  logger <- ask
  when (level >= loggerMinLevel logger) $ do
    timestamp <- liftIO getCurrentTime
    let message = LogMessage
          { messageTimestamp = timestamp
          , messageLevel = level
          , messageComponent = component
          , messageText = text
          , messageMetadata = metadata
          }
    liftIO $ do
      hPutStrLn (loggerHandle logger) $ toS $ loggerFormat logger message
      hFlush (loggerHandle logger)

-- | Run a computation with logging
withLogging :: Logger -> ReaderT Logger m a -> m a
withLogging logger action = runReaderT action logger

-- | Format a log message as text
formatLogMessage :: LogMessage -> Text
formatLogMessage msg = mconcat
  [ formatTimestamp (messageTimestamp msg)
  , " ["
  , show (messageLevel msg)
  , "] "
  , messageComponent msg
  , ": "
  , messageText msg
  , formatMetadata (messageMetadata msg)
  ]

-- | Format a timestamp
formatTimestamp :: UTCTime -> Text
formatTimestamp = T.pack . show

-- | Format metadata
formatMetadata :: [(Text, Text)] -> Text
formatMetadata [] = ""
formatMetadata metadata = " {" <> T.intercalate ", " (map formatPair metadata) <> "}"
  where
    formatPair (k, v) = k <> "=" <> v 