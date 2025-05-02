{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}

module Kampong.Error
  ( -- * Error Types
    KampongError(..)
  , StorageError(..)
  , NetworkError(..)
  , ValidationError(..)
    
    -- * Error Handling
  , throwKampongError
  , handleError
  , mapError
  ) where

import Protolude
import Data.Aeson (FromJSON, ToJSON)

-- | Main error type for the Kampong library
data KampongError
  = StorageError StorageError
  | NetworkError NetworkError
  | ValidationError ValidationError
  | InternalError Text
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Storage-related errors
newtype StorageError
  = StorageOperationError Text
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Network-related errors
data NetworkError
  = NetworkConnectionError Text
  | NetworkTimeoutError Text
  | NetworkInvalidResponse Text
  | NetworkAuthenticationError Text
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Validation-related errors
data ValidationError
  = ValidationInvalidInput Text
  | ValidationMissingField Text
  | ValidationInvalidFormat Text
  | ValidationConstraintViolation Text
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Throw an error in the current monad
throwKampongError :: (MonadError KampongError m) => KampongError -> m a
throwKampongError = throwError

-- | Handle errors in a computation
handleError :: (MonadError KampongError m) => (KampongError -> m a) -> m a -> m a
handleError handler action = catchError action handler

-- | Map one error type to another
mapError :: (MonadError e2 m) => (e1 -> e2) -> ExceptT e1 m a -> m a
mapError f = runExceptT >=> either (throwError . f) pure 