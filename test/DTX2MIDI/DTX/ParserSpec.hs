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
    it "parse a header" $ do
      ("#TITLE: FOO BAR\n" :: Text) ~> parseHeaderLine `shouldParse` (Header {headerKey = "TITLE", headerChannel = "", headerValue = "FOO BAR"})

    it "parse a header with channel" $ do
      ("#WAV1Z: foo-bar1z.xa\n" :: Text) ~> parseHeaderLine `shouldParse` (Header {headerKey = "WAV", headerChannel = "1Z", headerValue = "foo-bar1z.xa"})

    -- TODO
    -- it "parse a header with comment" $ do
    --   ("#WAV0: foo-bar0.xa ;kick" :: Text) ~> parseHeaderLine `shouldParse` (Header { headerKey = "WAV", headerChannel = "0", headerValue = "foo-bar0.xa" })

    it "parse a header without value" $ do
      ("#DTXC_CHIPPALETTE: \n" :: Text) ~> parseHeaderLine `shouldParse` (Header {headerKey = "DTXC_CHIPPALETTE", headerChannel = "", headerValue = ""})

    it "parse a header without ':'" $ do
      ("#BPM 84\n" :: Text) ~> parseHeaderLine `shouldParse` (Header {headerKey = "BPM", headerChannel = "", headerValue = "84"})

  describe "parseObjectLine" $ do
    it "parse an object" $ do
      ("#00111: 0101010101010101\n" :: Text) ~> parseObjectLine `shouldParse` (Object {objectKey = "001", objectChannel = "11", objectValue = ["01", "01", "01", "01", "01", "01", "01", "01"]})

    it "parse an object without space" $ do
      ("#00112:02000200\n" :: Text) ~> parseObjectLine `shouldParse` (Object {objectKey = "001", objectChannel = "12", objectValue = ["02", "00", "02", "00"]})

    -- TODO
    -- it "parse an object with comment" $ do
    --  ("#00111: 0101010101010101 ;intro" :: Text) ~> parseObjectLine `shouldParse` (Object { objectKey = "001", objectChannel = "11", objectValue = ["01", "01", "01", "01", "01", "01", "01", "01"] })

    it "parse an object with placeholders" $ do
      ("#00112 0002_____0002\n" :: Text) ~> parseObjectLine `shouldParse` (Object {objectKey = "001", objectChannel = "12", objectValue = ["00", "02", "00", "02"]})

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
              LineHeader (Header {headerKey = "TITLE", headerChannel = "", headerValue = "Moon Swimming Weekender"}),
              LineHeader (Header {headerKey = "BPM", headerChannel = "", headerValue = "136"}),
              LineComment "",
              LineObject (Object {objectKey = "001", objectChannel = "16", objectValue = ["16", "00", "00", "00"]}),
              LineObject (Object {objectKey = "001", objectChannel = "13", objectValue = ["13", "00", "13", "00", "00", "00", "00", "13", "00", "00", "13", "00", "00", "00", "00", "00"]}),
              LineObject (Object {objectKey = "001", objectChannel = "12", objectValue = ["00", "00", "00", "00", "12", "00", "12", "00", "00", "12", "00", "00", "12", "12", "00", "12"]}),
              LineObject (Object {objectKey = "001", objectChannel = "18", objectValue = ["00", "18", "18", "18", "18", "18", "18", "18"]})
            ]
      ( "; Created by DTXCreator\n\
        \\n\
        \#TITLE: Moon Swimming Weekender\n\
        \#BPM: 136\n\
        \\n\
        \#00116: 16000000\n\
        \#00113: 13001300000000130000130000000000\n\
        \#00112: 00000000120012000012000012120012\n\
        \#00118: 0018181818181818\n" ::
          Text
        )
        ~> parseLines
        `shouldParse` (expected)
