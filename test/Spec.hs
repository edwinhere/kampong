{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Data.Aeson as A
import Data.ByteString.Lazy as BL

import Network.ActivityPub.Vocabulary.Core as C
import Network.ActivityPub.Aeson.Core as C

import Debug.Trace

main :: IO ()
main = do
    apObject <- BL.readFile "test/examples/example-1.json"
    apLink <- BL.readFile "test/examples/example-2.json"
    hspec $ do
        describe "Decode Vocabulary" $ do
            it "should decode Object" $
                (decode apObject :: Maybe C.Object) `shouldNotBe` Nothing
            it "should decode Link" $
                traceShowId (A.eitherDecode apLink :: Either String C.Link) `shouldNotBe` (Left "")
        describe "Encode Vocabulary" $ do
            it "should encode Object" $
                1 `shouldBe` 1
            it "should encode Link" $
                1 `shouldBe` 1