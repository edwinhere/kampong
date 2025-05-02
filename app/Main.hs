{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Protolude
import Kampong
import qualified Data.Text as T

main :: IO ()
main = do
  -- Create a logger
  logger <- createLogger Info
  
  -- Run the server with logging
  withLogging logger $ do
    -- Create a SQLite storage backend
    storage <- createSQLiteStorage "kampong.db"
    
    -- Configure the server
    let config = ServerConfig
          { serverHost = "localhost"
          , serverPort = 8080
          , serverStorage = storage
          , serverActor = Actor
              { actorId = ActorId "server"
              , actorType = Application
              , actorName = "Kampong Server"
              , actorPreferredUsername = "server"
              , actorEndpoints = ActorEndpoints
                  { endpointInbox = URI "http://localhost:8080/inbox"
                  , endpointOutbox = URI "http://localhost:8080/outbox"
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
          }
    
    -- Start the server with error handling
    result <- runExceptT $ handleError
      (\err -> do
        logMessage Error "Main" "Server error occurred"
          [("error", show err)]
        throwError err)
      $ do
        logMessage Info "Main" "Starting server"
          [ ("host", serverHost config)
          , ("port", T.pack $ show $ serverPort config)
          , ("actor", unActorId $ actorId $ serverActor config)
          ]
        result <- liftIO $ try $ runServer config
        case result of
          Left (ioe :: IOException) -> 
            throwError $ InternalError $ T.pack $ show ioe
          Right _ -> pure ()
    
    case result of
      Left err -> do
        logMessage Error "Main" "Fatal server error"
          [("error", show err)]
        liftIO exitFailure
      Right _ -> pure ()
