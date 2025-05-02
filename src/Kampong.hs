{-# LANGUAGE NoImplicitPrelude #-}

module Kampong
  ( -- * Core Types
    module Kampong.Core
  , module Kampong.Core.Activity
  , module Kampong.Core.Actor
  , module Kampong.Core.Object
  , module Kampong.Core.Collection
  , module Kampong.Core.WebFinger
  , module Kampong.Core.Types
  , module Kampong.Core.Actor.Types
    
    -- * Server
  , module Kampong.Server
    
    -- * Storage
  , module Kampong.Storage
  , module Kampong.Storage.SQLite
    
    -- * Error Handling
  , module Kampong.Error
    
    -- * Logging
  , module Kampong.Logging
    
    -- * Example Usage
    -- $example
  ) where

import Kampong.Core
import Kampong.Core.Activity
import Kampong.Core.Actor
import Kampong.Core.Object
import Kampong.Core.Collection
import Kampong.Core.WebFinger
import Kampong.Core.Types
import Kampong.Core.Actor.Types
import Kampong.Server
import Kampong.Storage
import Kampong.Storage.SQLite
import Kampong.Error
import Kampong.Logging

{- $example
Here's a simple example of how to use Kampong to create a basic ActivityPub server:

@
import Kampong
import Kampong.Storage
import Kampong.Server
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
  -- Create an in-memory storage backend
  storage <- createStorage
  
  -- Configure the server
  let config = ServerConfig
        { serverHost = "example.com"
        , serverPort = 8080
        , serverStorage = storage
        , serverActor = Actor
            { actorId = "https://example.com/users/alice"
            , actorType = Person
            , actorName = "Alice"
            , actorPreferredUsername = "alice"
            , actorInbox = "https://example.com/users/alice/inbox"
            , actorOutbox = "https://example.com/users/alice/outbox"
            }
        }
  
  -- Start the server
  runServer config
@
-} 