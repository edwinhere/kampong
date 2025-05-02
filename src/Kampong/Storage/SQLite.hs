{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Kampong.Storage.SQLite
  ( -- * SQLite Storage
    createSQLiteStorage
  ) where

import Protolude
import Data.IORef (newIORef)
import Database.SQLite.Simple (Connection, open, execute_)
import Kampong.Storage (Storage(..))

-- | Create a new SQLite storage instance
createSQLiteStorage :: MonadIO m => FilePath -> m Storage
createSQLiteStorage path = do
  conn <- liftIO $ open path
  liftIO $ initializeDatabase conn
  actorsRef <- liftIO $ newIORef []
  objectsRef <- liftIO $ newIORef []
  activitiesRef <- liftIO $ newIORef []
  collectionsRef <- liftIO $ newIORef []
  pure $ Storage actorsRef objectsRef activitiesRef collectionsRef

-- | Initialize the database schema
initializeDatabase :: Connection -> IO ()
initializeDatabase conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS actors (id TEXT PRIMARY KEY, data TEXT NOT NULL)"
  execute_ conn "CREATE TABLE IF NOT EXISTS objects (id TEXT PRIMARY KEY, data TEXT NOT NULL)"
  execute_ conn "CREATE TABLE IF NOT EXISTS activities (id TEXT PRIMARY KEY, data TEXT NOT NULL)"
  execute_ conn "CREATE TABLE IF NOT EXISTS collections (id TEXT PRIMARY KEY, data TEXT NOT NULL)" 