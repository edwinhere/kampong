{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.ActivityPub.Aeson.Core where

import           Data.Maybe                     ( Maybe )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Data.Time                      ( UTCTime )
import           Network.URI                    ( parseURIReference
                                                , URI
                                                , uriToString
                                                )

-- import           Network.ActivityPub.Vocabulary.Properties
import           Network.ActivityPub.Vocabulary.Core
                                               as C

import           Data.Aeson                    as A
import           Data.Aeson.Types

import           Data.Foldable                  ( asum )

import           Text.Printf

import           Control.Monad

import           Control.Applicative

instance FromJSON URI where
    parseJSON =
        withText "URI"
            $ maybe (fail "invalid URI") return
            . parseURIReference
            . unpack

instance ToJSON URI where
    toJSON u = toJSON $ uriToString id u ""

-- data Object =
--     Object
--     { oid               :: URI
--     , oObjectProperties :: ObjectProperties
--     } deriving (Show, Eq)

instance FromJSON C.Object where
    parseJSON = withObject "Object" $ \o -> do
        oid               <- o .:? "id"
        oObjectProperties <- parseJSON (A.Object o)
        return C.Object { .. }

-- type Document = Object
-- type Image = Document

-- data Link =
--     Link
--     { lid             :: URI
--     , lLinkProperties :: LinkProperties
--     } deriving (Show, Eq)

instance FromJSON Link where
    parseJSON = withObject "Link" $ \o -> do
        lid             <- o .: "href" <|> o .:? "id"
        lLinkProperties <- parseJSON (A.Object o)
        return Link { .. }

-- data Activity =
--     Activity
--     { aid                 :: URI
--     , aObjectProperties   :: ObjectProperties
--     , aActivityProperties :: ActivityProperties
--     } deriving (Show, Eq)

instance FromJSON Activity where
    parseJSON = withObject "Activity" $ \o -> do
        aid                 <- o .:? "id"
        aObjectProperties   <- parseJSON (A.Object o)
        aActivityProperties <- parseJSON (A.Object o)
        return Activity { .. }

-- data Collection =
--     Collection
--     { cid                   :: URI
--     , cObjectProperties     :: ObjectProperties
--     , cCollectionProperties :: CollectionProperties
--     } deriving (Show, Eq)

instance FromJSON Collection where
    parseJSON = withObject "Collection" $ \o -> do
        cid                   <- o .:? "id"
        cObjectProperties     <- parseJSON (A.Object o)
        cCollectionProperties <- parseJSON (A.Object o)
        return Collection { .. }

-- type OrderedCollection = Collection

-- data CollectionPage =
--     CollectionPage
--     { cpid                       :: URI
--     , cpObjectProperties         :: ObjectProperties
--     , cpCollectionProperties     :: CollectionProperties
--     , cpCollectionPageProperties :: CollectionPageProperties
--     } deriving (Show, Eq)

instance FromJSON CollectionPage where
    parseJSON = withObject "CollectionPage" $ \o -> do
        cpid                       <- o .:? "id"
        cpObjectProperties         <- parseJSON (A.Object o)
        cpCollectionProperties     <- parseJSON (A.Object o)
        cpCollectionPageProperties <- parseJSON (A.Object o)
        return CollectionPage { .. }


-- data OrderedCollectionPage =
--     OrderedCollectionPage
--     { ocpid                       :: URI
--     , ocpObjectProperties         :: ObjectProperties
--     , ocpCollectionProperties     :: CollectionProperties
--     , ocpCollectionPageProperties :: CollectionPageProperties
--     , startIndex                  :: Maybe StartIndex
--     } deriving (Show, Eq)

instance FromJSON OrderedCollectionPage where
    parseJSON = withObject "OrderedCollectionPage" $ \o -> do
        ocpid                       <- o .:? "id"
        ocpObjectProperties         <- parseJSON (A.Object o)
        ocpCollectionProperties     <- parseJSON (A.Object o)
        ocpCollectionPageProperties <- parseJSON (A.Object o)
        startIndex                  <- o .:? "startIndex"
        return OrderedCollectionPage { .. }

-- -- Property Data Types
-- data CollectionPageProperties =
--     CollectionPageProperties
--     { partOf :: Maybe PartOf
--     , next   :: Maybe Next
--     , prev   :: Maybe Prev
--     } deriving (Show, Eq)

instance FromJSON CollectionPageProperties where
    parseJSON = withObject "CollectionPageProperties" $ \o -> do
        partOf <- o .:? "partOf"
        next   <- o .:? "next"
        prev   <- o .:? "prev"
        return CollectionPageProperties { .. }

-- data CollectionProperties =
--     CollectionProperties
--     { totalItems :: Maybe TotalItems
--     , current    :: Maybe Current
--     , first      :: Maybe First
--     , last       :: Maybe Last
--     , items      :: Maybe Items
--     } deriving (Show, Eq)

instance FromJSON CollectionProperties where
    parseJSON = withObject "CollectionProperties" $ \o -> do
        totalItems <- o .:? "totalItems"
        current    <- o .:? "current"
        first      <- o .:? "first"
        last       <- o .:? "last"
        items      <- o .:? "items"
        return CollectionProperties { .. }

-- data LinkProperties =
--     LinkProperties
--     { href      :: Maybe Href
--     , rel       :: Maybe Rel
--     , mediaType :: Maybe MediaType
--     , name      :: Maybe Name
--     , hreflang  :: Maybe HrefLang
--     , height    :: Maybe Height
--     , width     :: Maybe Width
--     , preview   :: Maybe Preview
--     } deriving (Show, Eq)

instance FromJSON LinkProperties where
    parseJSON = withObject "LinkProperties" $ \o -> do
        href      <- o .:? "href"
        rel       <- o .:? "rel"
        mediaType <- o .:? "mediaType"
        name      <- o .:? "name"
        hreflang  <- o .:? "hreflang"
        height    <- o .:? "height"
        width     <- o .:? "width"
        preview   <- o .:? "preview"
        return LinkProperties { .. }

-- data ActivityProperties =
--     ActivityProperties
--     { actor      :: Maybe Actor
--     , object     :: Maybe ObjectRef
--     , target     :: Maybe Target
--     , result     :: Maybe Result
--     , origin     :: Maybe Origin
--     , instrument :: Maybe Instrument
--     } deriving (Show, Eq)

instance FromJSON ActivityProperties where
    parseJSON = withObject "ActivityProperties" $ \o -> do
        actor      <- o .:? "actor"
        object     <- o .:? "object"
        target     <- o .:? "target"
        result     <- o .:? "result"
        origin     <- o .:? "origin"
        instrument <- o .:? "instrument"
        return ActivityProperties { .. }

-- data ObjectProperties =
--     ObjectProperties
--     { attachment   :: Maybe Attachment
--     , attributedTo :: Maybe AttributedTo
--     , audience     :: Maybe Audience
--     , content      :: Maybe Content
--     , context      :: Maybe Context
--     , objName      :: Maybe Name
--     , endTime      :: Maybe EndTime
--     , generator    :: Maybe Generator
--     , icon         :: Maybe Icon
--     , image        :: Maybe ImageRef
--     , inReplyTo    :: Maybe InReplyTo
--     , location     :: Maybe Location
--     , objPreview   :: Maybe Preview
--     , published    :: Maybe Published
--     , replies      :: Maybe Replies
--     , startTime    :: Maybe StartTime
--     , summary      :: Maybe Summary
--     , tag          :: Maybe Tag
--     , updated      :: Maybe Updated
--     , url          :: Maybe URL
--     , to           :: Maybe To
--     , bto          :: Maybe Bto
--     , cc           :: Maybe Cc
--     , bcc          :: Maybe Bcc
--     , objMediaType :: Maybe MediaType
--     , duration     :: Maybe Duration
--     } deriving (Show, Eq)

instance FromJSON ObjectProperties where
    parseJSON = withObject "ObjectProperties" $ \o -> do
        attachment   <- o .:? "attachment"
        attributedTo <- o .:? "attributedTo"
        audience     <- o .:? "audience"
        content      <- o .:? "content"
        context      <- o .:? "context"
        objName      <- o .:? "name"
        endTime      <- o .:? "endTime"
        generator    <- o .:? "generator"
        icon         <- o .:? "icon"
        image        <- o .:? "image"
        inReplyTo    <- o .:? "inReplyTo"
        location     <- o .:? "location"
        objPreview   <- o .:? "preview"
        published    <- o .:? "published"
        replies      <- o .:? "replies"
        startTime    <- o .:? "startTime"
        summary      <- o .:? "summary"
        tag          <- o .:? "tag"
        updated      <- o .:? "updated"
        url          <- o .:? "url"
        to           <- o .:? "to"
        bto          <- o .:? "bto"
        cc           <- o .:? "cc"
        bcc          <- o .:? "bcc"
        objMediaType <- o .:? "mediaType"
        duration     <- o .:? "duration"
        return ObjectProperties { .. }


-- data Items = Item Object | ItemRef Link | Items [Either Object Link] deriving (Show, Eq)

instance FromJSON Items where
    parseJSON = withObject "Item or ItemRef or Items" $ \o -> asum
        [ Item <$> parseJSON (A.Object o)
        , ItemRef <$> parseJSON (A.Object o)
        , Items <$> parseJSON (A.Object o)
        ]

-- data Closed = ClosedObject Object | ClosedLink Link | ClosedTime UTCTime | ClosedState Bool deriving (Show, Eq)

instance FromJSON Closed where
    parseJSON =
        withObject "ClosedObject or ClosedLink or ClosedTime or ClosedState"
            $ \o -> asum
                  [ ClosedObject <$> parseJSON (A.Object o)
                  , ClosedLink <$> parseJSON (A.Object o)
                  , ClosedTime <$> parseJSON (A.Object o)
                  , ClosedState <$> parseJSON (A.Object o)
                  ]

-- type Actor = Either Object Link
-- type Attachment = Either Object Link
-- type AttributedTo = Either Object Link
-- type Audience = Either Object Link
-- type Bcc = Either Object Link
-- type Bto = Either Object Link
-- type Cc = Either Object Link
-- type Context = Either Object Link
-- type Current = Either CollectionPage Link
-- type First = Either CollectionPage Link
-- type Generator = Either Object Link
-- type Icon = Either Image Link
-- type ImageRef = Either Image Link
-- type InReplyTo = Either Object Link
-- type Instrument = Either Object Link
-- type Last = Either CollectionPage Link
-- type Location = Either Object Link
-- type OneOf = Either Object Link
-- type AnyOf = Either Object Link
-- type Origin = Either Object Link
-- type Next = Either CollectionPage Link
-- type ObjectRef = Either Object Link
-- type Prev = Either CollectionPage Link
-- type Preview = Either Object Link
-- type Result = Either Object Link
-- type Replies = Collection
-- type Tag = Either Object Link
-- type Target = Either Object Link
-- type To = Either Object Link
-- type URL = Either URI Link
-- type PartOf = Either Link Collection
-- type Units = Either Unit URI
-- type Subject = Either Object Link
-- type Describes = Object
-- type FormerType = Object
