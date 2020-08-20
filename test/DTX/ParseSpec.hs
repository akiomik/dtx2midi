{-# LANGUAGE OverloadedStrings #-}

module DTX.ParseSpec where

import Test.Hspec
import Test.Hspec.Attoparsec

import Data.Text

import DTX.Parse

spec :: Spec
spec = do
  describe "parseHeader" $ do
    it "parse a header" $ do
      ("#TITLE: FOO BAR" :: Text) ~> parseHeader `shouldParse` (Header { headerKey = "TITLE", headerChannel = "", headerValue = "FOO BAR" })

    it "parse a header with channel" $ do
      ("#WAV1Z: foo-bar1z.xa" :: Text) ~> parseHeader `shouldParse` (Header { headerKey = "WAV", headerChannel = "1Z", headerValue = "foo-bar1z.xa" })

    -- TODO
    -- it "parse a header with comment" $ do
    --   ("#WAV0: foo-bar0.xa ;kick" :: Text) ~> parseHeader `shouldParse` (Header { headerKey = "WAV", headerChannel = "0", headerValue = "foo-bar0.xa" })

    it "parse a header without value" $ do
      ("#DTXC_CHIPPALETTE: " :: Text) ~> parseHeader `shouldParse` (Header { headerKey = "DTXC_CHIPPALETTE", headerChannel = "", headerValue = "" })

    it "parse a header without ':'" $ do
      ("#BPM 84" :: Text) ~> parseHeader `shouldParse` (Header { headerKey = "BPM", headerChannel = "", headerValue = "84" })

  describe "parseObject" $ do
    it "parse an object" $ do
      ("#00111: 0101010101010101" :: Text) ~> parseObject `shouldParse` (Object { objectKey = "001", objectChannel = "11", objectValue = "0101010101010101" })

    it "parse an object without space" $ do
      ("#00112:02000200" :: Text) ~> parseObject `shouldParse` (Object { objectKey = "001", objectChannel = "12", objectValue = "02000200" })

    -- TODO
    -- it "parse an object with comment" $ do
    --  ("#00111: 0101010101010101 ;intro" :: Text) ~> parseObject `shouldParse` (Object { objectKey = "001", objectChannel = "11", objectValue = "0101010101010101" })

  describe "parseComment" $ do
    it "parse a comment line" $ do
      (";chorus" :: Text) ~> parseComment `shouldParse` ("chorus")

  describe "parseBlank" $ do
    it "parse a blank line" $ do
      ("\n" :: Text) ~> parseBlank `shouldParse` ("")

  describe "parseLines" $ do
    it "parse a dtx file" $ do
      let expected = [ LineComment " Created by DTXCreator"
                     , LineComment ""
                     , LineComment ""
                     , LineHeader (Header { headerKey = "TITLE", headerChannel = "", headerValue = "Moon Swimming Weekender" })
                     , LineComment ""
                     , LineHeader (Header { headerKey = "BPM", headerChannel = "", headerValue = "136" })
                     , LineComment ""
                     , LineComment ""
                     , LineObject (Object { objectKey = "001", objectChannel = "16", objectValue = "16000000" })
                     , LineComment ""
                     , LineObject (Object { objectKey = "001", objectChannel = "13", objectValue = "13001300000000130000130000000000" })
                     , LineComment ""
                     , LineObject (Object { objectKey = "001", objectChannel = "12", objectValue = "00000000120012000012000012120012" })
                     , LineComment ""
                     , LineObject (Object { objectKey = "001", objectChannel = "18", objectValue = "0018181818181818" })
                     , LineComment ""]
      ("; Created by DTXCreator\n\
       \\n\
       \#TITLE: Moon Swimming Weekender\n\
       \#BPM: 136\n\
       \\n\
       \#00116: 16000000\n\
       \#00113: 13001300000000130000130000000000\n\
       \#00112: 00000000120012000012000012120012\n\
       \#00118: 0018181818181818\n" :: Text) ~> parseLines `shouldParse` (expected)
