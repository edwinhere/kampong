module Main (main) where

import Test.Hspec
import Kampong
import Kampong.Storage.Interface
import Kampong.Storage.Types as StorageTypes
import qualified Data.Text as T
import Control.Monad.Reader (runReaderT)

main :: IO ()
main = hspec $ do
  describe "Kampong" $ do
    describe "Core" $ do
      describe "Actor" $ do
        it "can create a valid actor" $ do
          let actor = Actor
                { actorId = ActorId (T.pack "https://example.com/users/test")
                , actorType = Person
                , actorName = T.pack "Test User"
                , actorPreferredUsername = T.pack "test"
                , actorEndpoints = ActorEndpoints
                    { endpointInbox = URI (T.pack "https://example.com/users/test/inbox")
                    , endpointOutbox = URI (T.pack "https://example.com/users/test/outbox")
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
          actorId actor `shouldBe` ActorId (T.pack "https://example.com/users/test")
          actorType actor `shouldBe` Person
          actorName actor `shouldBe` T.pack "Test User"

      describe "Activity" $ do
        it "can create a valid activity" $ do
          let activity = Activity
                { activityId = ActivityId (T.pack "https://example.com/activities/1")
                , activityType = Create
                , activityActor = ActorId (T.pack "https://example.com/users/test")
                , activityObject = ObjectRef (ObjectId (T.pack "https://example.com/objects/1"))
                , activityTo = []
                , activityCc = []
                , activityBto = []
                , activityBcc = []
                , activityAudience = []
                , activityPublished = Nothing
                , activityUpdated = Nothing
                }
          activityId activity `shouldBe` ActivityId (T.pack "https://example.com/activities/1")
          activityType activity `shouldBe` Create
          activityActor activity `shouldBe` ActorId (T.pack "https://example.com/users/test")

      describe "Object" $ do
        it "can create a valid object" $ do
          let obj = Object
                { objectId = ObjectId (T.pack "https://example.com/objects/1")
                , objectType = Note
                , objectActor = ActorId (T.pack "https://example.com/users/test")
                , objectContent = T.pack "Hello, world!"
                , objectAttachments = []
                , objectTo = []
                , objectCc = []
                , objectBto = []
                , objectBcc = []
                , objectAudience = []
                , objectPublished = Nothing
                , objectUpdated = Nothing
                , objectInReplyTo = Nothing
                , objectUrl = Nothing
                , objectTag = []
                , objectReplies = Nothing
                , objectSensitive = False
                , objectSummary = Nothing
                }
          objectId obj `shouldBe` ObjectId (T.pack "https://example.com/objects/1")
          objectType obj `shouldBe` Note
          objectContent obj `shouldBe` T.pack "Hello, world!"

    describe "Storage" $ do
      describe "Memory Storage" $ do
        it "can store and retrieve an actor" $ do
          storage <- createStorage
          let storageInterface = createStorageInterface storage
          let actor = Actor
                { actorId = ActorId (T.pack "https://example.com/users/test")
                , actorType = Person
                , actorName = T.pack "Test User"
                , actorPreferredUsername = T.pack "test"
                , actorEndpoints = ActorEndpoints
                    { endpointInbox = URI (T.pack "https://example.com/users/test/inbox")
                    , endpointOutbox = URI (T.pack "https://example.com/users/test/outbox")
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
          
          -- Create actor
          result <- storageCreateActor storageInterface actor
          result `shouldBe` Right actor
          
          -- Retrieve actor
          retrieved <- storageGetActor storageInterface (actorId actor)
          retrieved `shouldBe` Right (Just actor)
          
          -- Update actor
          let updatedActor = actor { actorName = T.pack "Updated User" }
          updateResult <- storageUpdateActor storageInterface updatedActor
          updateResult `shouldBe` Right updatedActor
          
          -- Delete actor
          deleteResult <- storageDeleteActor storageInterface (actorId actor)
          deleteResult `shouldBe` Right True
          
          -- Verify deletion
          finalResult <- storageGetActor storageInterface (actorId actor)
          finalResult `shouldBe` Right Nothing

    describe "WebFinger" $ do
      it "can create a valid WebFinger resource" $ do
        logger <- createLogger Info
        resource <- runReaderT (createWebFingerResource
              (T.pack "acct:test@example.com")
              [T.pack "https://example.com/users/test"]
              [ createActorLink (URI (T.pack "https://example.com/users/test"))
              , createProfileLink (URI (T.pack "https://example.com/users/test/profile"))
              , createInboxLink (URI (T.pack "https://example.com/users/test/inbox"))
              , createOutboxLink (URI (T.pack "https://example.com/users/test/outbox"))
              ]
              []) logger
        
        resourceSubject resource `shouldBe` T.pack "acct:test@example.com"
        length (resourceLinks resource) `shouldBe` 4
        any (\link -> linkRel link == T.pack "self") (resourceLinks resource) `shouldBe` True

    describe "Error Handling" $ do
      it "handles storage errors correctly" $ do
        storage <- createStorage
        let storageInterface = createStorageInterface storage
        let actor = Actor
              { actorId = ActorId (T.pack "https://example.com/users/test")
              , actorType = Person
              , actorName = T.pack "Test User"
              , actorPreferredUsername = T.pack "test"
              , actorEndpoints = ActorEndpoints
                  { endpointInbox = URI (T.pack "https://example.com/users/test/inbox")
                  , endpointOutbox = URI (T.pack "https://example.com/users/test/outbox")
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
        
        -- Create actor
        _ <- storageCreateActor storageInterface actor
        
        -- Try to create the same actor again
        result <- storageCreateActor storageInterface actor
        case result of
          Left (StorageTypes.StorageError _) -> True `shouldBe` True
          _ -> False `shouldBe` True

    describe "Logging" $ do
      it "can create and use a logger" $ do
        logger <- createLogger Info
        runReaderT (do
          logMessage Info (T.pack "Test") (T.pack "Test message") [(T.pack "key", T.pack "value")]
          pure True) logger `shouldReturn` True
