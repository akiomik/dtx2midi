{-# LANGUAGE OverloadedStrings #-}

module DTX2MIDI.DTX.Parser
  ( readFile,
    parseText,
    -- for testing
    parseHeaderLine,
    parseObjectLine,
    parseCommentLine,
    parseBlankLine,
    parseLines,
  )
where

import Control.Applicative
import Control.Monad.Trans.Resource (MonadThrow, runResourceT)
import DTX2MIDI.DTX
import Data.Attoparsec.Text
import Data.ByteString (ByteString)
import Data.Char (isSpace)
import Data.Conduit (ConduitT, Void, runConduit, (.|))
import Data.Conduit.Attoparsec (sinkParser)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit.Text (decode, utf8)
import Data.Text (Text, pack, singleton)
import Prelude hiding (readFile, take)

spaceWithoutEOL :: Parser Char
spaceWithoutEOL =
  satisfy isSpaceWithoutEOL
  where
    isSpaceWithoutEOL c = (isSpace c) && (not $ isEndOfLine c)

parseHeaderKey :: Parser Key
parseHeaderKey =
  string "TITLE"
    <|> string "ARTIST"
    <|> string "BPM"
    <|> string "BASEBPM"
    <|> string "DLEVEL"
    <|> string "GLEVEL"
    <|> string "BLEVEL"
    <|> string "DTXVPLAYSPEED"
    <|> string "BGMWAV"
    <|> string "WAV"
    <|> string "VOLUME"
    <|> string "PAN"
    <|> string "STAGEFILE"
    <|> string "PREVIEW"
    <|> string "PREIMAGE"
    <|> string "PREMOVIE"
    <|> string "SOUND_NOWLOADING"
    <|> string "SOUND_STAGEFAILED"
    <|> string "SOUND_FULLCOMBO"
    <|> string "RESULTIMAGE"
    <|> string "RESULTMOVIE"
    <|> string "RESULTSOUND"
    <|> string "BACKGROUND"
    <|> string "BACKGROUND_GR"
    <|> string "WALL"
    <|> string "BMP"
    <|> string "GENRE"
    <|> string "DTXC_CHIPPALETTE"
    <|> string "DTXC_LANEBINDEDCHIP"

parseChannel :: Parser Text
parseChannel = takeTill (\w -> w == ' ' || w == ':')

parseHeaderValue :: Parser Text
parseHeaderValue = takeTill isEndOfLine

parseHeader :: Parser Header
parseHeader = do
  char '#'
  key <- parseHeaderKey
  chan <- parseChannel
  option ':' $ char ':'
  skipMany spaceWithoutEOL
  value <- parseHeaderValue
  Header <$> pure key <*> pure chan <*> pure value

parseHeaderLine :: Parser Header
parseHeaderLine = parseHeader <* endOfLine

parseObjectKey :: Parser Key
parseObjectKey = do
  n1 <- satisfy $ inClass "0-9A-Z"
  n_ <- count 2 digit
  return $ pack $ n1 : n_

parseNoteObjectValue :: Parser [Note]
parseNoteObjectValue = do
  (filter (/= ("_" :: Text))) <$> many (parsePlaceHolder <|> parseNote)
  where
    parseNote = pack <$> (count 2 $ satisfy $ inClass "0-9A-Z")
    parsePlaceHolder = singleton <$> char '_'

parseUnsupportedEventObjectValue :: Parser Text
parseUnsupportedEventObjectValue = takeTill isEndOfLine

isNoteEvent :: Channel -> Bool
isNoteEvent chan
  | chan `elem` ["11", "12", "13", "14", "15", "16", "17", "18", "19", "1A", "1B", "1C"] = True
  | otherwise = False

noteEventToObjectValue :: Channel -> [Note] -> ObjectValue
noteEventToObjectValue "11" = HiHatClose
noteEventToObjectValue "12" = Snare
noteEventToObjectValue "13" = BassDrum
noteEventToObjectValue "14" = HighTom
noteEventToObjectValue "15" = LowTom
noteEventToObjectValue "16" = Cymbal
noteEventToObjectValue "17" = FloorTom
noteEventToObjectValue "18" = HiHatOpen
noteEventToObjectValue "19" = RideCymbal
noteEventToObjectValue "1A" = LeftCymbal
noteEventToObjectValue "1B" = LeftPedal
noteEventToObjectValue "1C" = LeftBassDrum

parseObject :: Parser Object
parseObject = do
  char '#'
  key <- parseObjectKey
  chan <- parseChannel
  option ':' $ char ':'
  skipMany spaceWithoutEOL
  if isNoteEvent chan
    then do
      value <- parseNoteObjectValue
      Object <$> pure key <*> pure (noteEventToObjectValue chan value)
    else do
      value <- parseUnsupportedEventObjectValue
      Object <$> pure key <*> pure (UnsupportedEvent chan value)

parseObjectLine :: Parser Object
parseObjectLine = parseObject <* endOfLine

parseComment :: Parser Comment
parseComment = do
  _ <- char ';'
  takeTill isEndOfLine

parseCommentLine :: Parser Comment
parseCommentLine = parseComment <* endOfLine

parseBlankLine :: Parser Comment
parseBlankLine = endOfLine >>= return ""

parseLine :: Parser Line
parseLine =
  fmap LineComment (parseCommentLine <|> parseBlankLine)
    <|> fmap LineHeader parseHeaderLine
    <|> fmap LineObject parseObjectLine

parseLines :: Parser [Line]
parseLines = many1 parseLine

readFile :: FilePath -> IO [Line]
readFile fp = runResourceT $ runConduit $ CB.sourceFile fp .| sinkLines

parseText :: Text -> IO [Line]
parseText t = runConduit $ CL.sourceList [t] .| sinkTextLines

sinkLines :: (MonadThrow m) => ConduitT ByteString Void m [Line]
sinkLines = decode utf8 .| sinkParser parseLines

sinkTextLines :: (MonadThrow m) => ConduitT Text Void m [Line]
sinkTextLines = sinkParser parseLines
