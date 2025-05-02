{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Kampong.Storage.Memory
  ( -- * Memory Storage
    createMemoryStorage
  ) where

import Protolude
import Kampong.Storage (Storage(..), createStorage)

-- | Create a new in-memory storage instance
createMemoryStorage :: MonadIO m => m Storage
createMemoryStorage = createStorage 