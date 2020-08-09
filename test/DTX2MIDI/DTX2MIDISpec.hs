{-# LANGUAGE OverloadedStrings #-}

module DTX2MIDI.DTX2MIDISpec where

import Control.Exception (evaluate)
import Test.Hspec
-- import qualified Haskore.Music as Music
import Haskore.Music.GeneralMIDI as GM

import DTX.Parse
import DTX2MIDI

spec :: Spec
spec = do
  describe "bpm" $ do
    it "returns bpm when BPM is defined in the headers" $ do
      let dtx = [ LineHeader $ Header "TITLE" "" "foo"
                , LineHeader $ Header "BPM" "" "100" ]
      bpm dtx `shouldBe` (Just 100)

    it "returns Nothing when BPM is not defined in the headers" $ do
      let dtx = [LineHeader $ Header "TITLE" "" "foo"]
      bpm dtx `shouldBe` (Nothing)

  describe "parseObjectValue" $ do
    it "returns parsed measure object values" $ do
      parseObjectValue "010203" `shouldBe` (["01", "02", "03"])

  describe "keyCompletion" $ do
    it "returns filled measure ids" $ do
      keyCompletion ["001", "005"] `shouldBe` (["001", "002", "003", "004", "005"])

  describe "objectCompletion" $ do
    it "returns missing measure objects that are filled with empty hi-hats" $ do
      objectCompletion ["001", "005"] `shouldBe` ([ Object "002" "11" "00"
                                                  , Object "003" "11" "00"
                                                  , Object "004" "11" "00" ])
