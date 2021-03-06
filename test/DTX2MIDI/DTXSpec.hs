{-# LANGUAGE OverloadedStrings #-}

module DTX2MIDI.DTXSpec where

import DTX2MIDI.DTX
import Test.Hspec

spec :: Spec
spec = do
  describe "headers" $ do
    it "returns headers from entire dtx file" $ do
      let dtx =
            [ LineHeader $ Header "TITLE" "" "foo",
              LineHeader $ Header "BPM" "" "100",
              LineComment "",
              LineObject $ Object "001" $ HiHatClose ["01", "01", "01", "01", "01", "01", "01", "01"],
              LineObject $ Object "001" $ Snare ["00", "02", "00", "02"],
              LineObject $ Object "001" $ BassDrum ["03", "00", "00", "00", "03", "03", "00", "00"]
            ]
      headers dtx `shouldBe` [Header "TITLE" "" "foo", Header "BPM" "" "100"]

  describe "objects" $ do
    it "returns objects from entire dtx file" $ do
      let dtx =
            [ LineHeader $ Header "TITLE" "" "foo",
              LineHeader $ Header "BPM" "" "100",
              LineComment "",
              LineObject $ Object "001" $ HiHatClose ["01", "01", "01", "01", "01", "01", "01", "01"],
              LineObject $ Object "001" $ Snare ["00", "02", "00", "02"],
              LineObject $ Object "001" $ BassDrum ["03", "00", "00", "00", "03", "03", "00", "00"]
            ]
      let expected =
            [ Object "001" $ HiHatClose ["01", "01", "01", "01", "01", "01", "01", "01"],
              Object "001" $ Snare ["00", "02", "00", "02"],
              Object "001" $ BassDrum ["03", "00", "00", "00", "03", "03", "00", "00"]
            ]
      objects dtx `shouldBe` expected

  describe "baseBPM" $ do
    it "returns base bpm when base BPM is defined in the headers" $ do
      let dtx =
            [ LineHeader $ Header "TITLE" "" "foo",
              LineHeader $ Header "BPM" "" "100"
            ]
      baseBPM dtx `shouldBe` Just 100

    it "returns Nothing when base BPM is not defined in the headers" $ do
      let dtx =
            [ LineHeader $ Header "TITLE" "" "foo",
              LineHeader $ Header "BPM" "01" "100"
            ]
      baseBPM dtx `shouldBe` Nothing

    it "returns Nothing when BPM is not defined in the headers" $ do
      let dtx = [LineHeader $ Header "TITLE" "" "foo"]
      baseBPM dtx `shouldBe` Nothing
