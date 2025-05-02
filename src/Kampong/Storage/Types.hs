{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Kampong.Storage.Types
  ( -- * Storage Types
    StorageError(..)
  ) where

import Protolude
import Data.Aeson (FromJSON, ToJSON)

-- | Storage-related errors
newtype StorageError
  = StorageError Text
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON) 