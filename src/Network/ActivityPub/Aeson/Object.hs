{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.ActivityPub.Aeson.Object where

-- import           Data.Maybe                     ( Maybe )
-- import           Network.URI                    ( URI )

-- import           Network.ActivityPub.Vocabulary.Core
-- import           Network.ActivityPub.Vocabulary.Properties
import           Network.ActivityPub.Vocabulary.Object
import           Network.ActivityPub.Aeson.Core
import           Network.ActivityPub.Aeson.Properties

import           Data.Aeson                    as A

-- Object Types
-- type Article = Object
-- type Event = Object
-- type Note = Object
-- type Audio = Document
-- type Page = Document
-- type Video = Document

-- data Tombstone = Tombstone
--             { tid               :: URI
--             , tObjectProperties :: ObjectProperties
--             , formerType        :: Maybe FormerType
--             , deleted           :: Maybe Deleted
--             } deriving (Show, Eq)

instance FromJSON Tombstone where
    parseJSON = withObject "Object" $ \o -> do
        tid               <- o .: "id"
        tObjectProperties <- parseJSON (A.Object o)
        formerType        <- o .:? "formerType"
        deleted           <- o .:? "deleted"
        return Tombstone { .. }

-- data Profile = Profile
--             { prid               :: URI
--             , prObjectProperties :: ObjectProperties
--             , describes          :: Maybe Describes
--             } deriving (Show, Eq)

instance FromJSON Profile where
    parseJSON = withObject "Profile" $ \o -> do
        prid               <- o .: "id"
        prObjectProperties <- parseJSON (A.Object o)
        describes          <- o .:? "describes"
        return Profile { .. }

-- data Place = Place
--             { pid               :: URI
--             , pObjectProperties :: ObjectProperties
--             , accuracy          :: Maybe Accuracy
--             , altitude          :: Maybe Altitude
--             , latitude          :: Maybe Latitude
--             , longitude         :: Maybe Longitude
--             , radius            :: Maybe Radius
--             , units             :: Maybe Units
--             } deriving (Show, Eq)

instance FromJSON Place where
    parseJSON = withObject "Place" $ \o -> do
        pid               <- o .: "id"
        pObjectProperties <- parseJSON (A.Object o)
        accuracy          <- o .:? "accuracy"
        altitude          <- o .:? "altitude"
        latitude          <- o .:? "latitude"
        longitude         <- o .:? "longitude"
        radius            <- o .:? "radius"
        units             <- o .:? "units"
        return Place { .. }

-- data Relationship = Relationship
--             { rid               :: URI
--             , rObjectProperties :: ObjectProperties
--             , subject           :: Maybe Subject
--             , object            :: Maybe ObjectRef
--             , relationship      :: Maybe Relationship
--             } deriving (Show, Eq)

-- Link Types
-- type Mention = Link
