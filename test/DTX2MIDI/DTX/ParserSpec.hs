{-# LANGUAGE OverloadedStrings #-}

module DTX2MIDI.DTX.ParserSpec where

import DTX2MIDI.DTX
import DTX2MIDI.DTX.Parser
import Data.Text
import Test.Hspec
import Test.Hspec.Attoparsec

spec :: Spec
spec = do
  describe "parseHeaderLine" $ do
    it "parse a known header" $ do
      ("#WAV: foo-bar.xa\n" :: Text) ~> parseHeaderLine `shouldParse` (Header "WAV" "" "foo-bar.xa")

    it "parse a known header with channel" $ do
      ("#WAV1Z: foo-bar1z.xa\n" :: Text) ~> parseHeaderLine `shouldParse` (Header "WAV" "1Z" "foo-bar1z.xa")

    it "parse an unknown header" $ do
      ("#FOOBAR: FOO BAR\n" :: Text) ~> parseHeaderLine `shouldParse` (Header "FOOBAR" "" "FOO BAR")

    it "parse a header with inline comment" $ do
      -- TODO: parse inline comment
      ("#WAV0: foo-bar0.xa ;kick\n" :: Text) ~> parseHeaderLine `shouldParse` (Header "WAV" "0" "foo-bar0.xa")

    it "parse a header without value" $ do
      ("#DTXC_CHIPPALETTE: \n" :: Text) ~> parseHeaderLine `shouldParse` (Header "DTXC_CHIPPALETTE" "" "")

    it "parse a header without ':'" $ do
      ("#BPM 84\n" :: Text) ~> parseHeaderLine `shouldParse` (Header "BPM" "" "84")

  describe "parseObjectLine" $ do
    it "parse an object" $ do
      ("#00111: 0101010101010101\n" :: Text) ~> parseObjectLine `shouldParse` (Object "001" $ HiHatClose ["01", "01", "01", "01", "01", "01", "01", "01"])

    it "parse an object without space" $ do
      ("#00112:02000200\n" :: Text) ~> parseObjectLine `shouldParse` (Object "001" $ Snare ["02", "00", "02", "00"])

    it "parse an object with inline comment" $ do
      -- TODO: parse inline comment
      ("#00111: 0101010101010101 ;intro\n" :: Text) ~> parseObjectLine `shouldParse` (Object "001" $ HiHatClose ["01", "01", "01", "01", "01", "01", "01", "01"])

    it "parse an object with placeholders" $ do
      ("#00112 0002_____0002\n" :: Text) ~> parseObjectLine `shouldParse` (Object "001" $ Snare ["00", "02", "00", "02"])

    it "parse an object with unsupported channel" $ do
      ("#00102 0.5\n" :: Text) ~> parseObjectLine `shouldParse` (Object "001" $ UnsupportedEvent "02" "0.5")

    it "parse an object with unsupported channel and inline comment" $ do
      ("#00102 0.5 ;half measure\n" :: Text) ~> parseObjectLine `shouldParse` (Object "001" $ UnsupportedEvent "02" "0.5")

  describe "parseCommentLine" $ do
    it "parse a comment line" $ do
      (";chorus\n" :: Text) ~> parseCommentLine `shouldParse` ("chorus")

  describe "parseBlankLine" $ do
    it "parse a blank line" $ do
      ("\n" :: Text) ~> parseBlankLine `shouldParse` ("")

  describe "parseLines" $ do
    it "parse a dtx file" $ do
      let expected =
            [ LineComment " Created by DTXCreator",
              LineComment "",
              LineHeader (Header "TITLE" "" "Moon Swimming Weekender"),
              LineHeader (Header "BPM" "" "136"),
              LineComment "",
              LineObject (Object "000" $ UnsupportedEvent "01" "000000000B"),
              LineObject (Object "001" $ Cymbal ["16", "00", "00", "00"]),
              LineObject (Object "001" $ BassDrum ["13", "00", "13", "00", "00", "00", "00", "13", "00", "00", "13", "00", "00", "00", "00", "00"]),
              LineObject (Object "001" $ Snare ["00", "00", "00", "00", "12", "00", "12", "00", "00", "12", "00", "00", "12", "12", "00", "12"]),
              LineObject (Object "001" $ HiHatOpen ["00", "18", "18", "18", "18", "18", "18", "18"])
            ]
      ( "; Created by DTXCreator\n\
        \\n\
        \#TITLE: Moon Swimming Weekender\n\
        \#BPM: 136\n\
        \\n\
        \#00001: 000000000B ;BGM\n\
        \#00116: 16000000\n\
        \#00113: 13001300000000130000130000000000\n\
        \#00112: 00000000120012000012000012120012\n\
        \#00118: 0018181818181818\n" ::
          Text
        )
        ~> parseLines
        `shouldParse` (expected)
