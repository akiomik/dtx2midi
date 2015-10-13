{-# LANGUAGE OverloadedStrings #-}

module DTX.Parse
    (
        readFile
      , parseText
      , Line(..)
      , Object(..)
      , Header(..)
      , Comment(..)
      , object
      , header
      , comment
      , isHeader
      , isComment
      , isObject
    ) where

import Control.Applicative
import Control.Monad.Trans.Resource (MonadThrow, runResourceT)
import Data.Attoparsec.Text
import Data.ByteString (ByteString)
import Data.Conduit (Conduit, Sink, ($$), ($=), (=$=), (=$))
import Data.Conduit.Attoparsec (sinkParser, conduitParser)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit.Text (decode, utf8)
import Data.Text (Text, pack)
import Prelude hiding (takeWhile, readFile)

type Channel = Text
type Comment = Text

data Header = Header
    {
        headerKey :: Text
      , headerChannel :: Channel
      , headerValue :: Text
    }
  deriving (Show, Eq)

-- TODO: objectValueの時点でノートをパースしたい
data Object = Object
    {
        objectKey :: Text
      , objectChannel :: Channel
      , objectValue :: Text
    }
  deriving (Show, Eq)

data Line =
    LineHeader Header
        | LineComment Comment
        | LineObject Object
  deriving (Show, Eq)

header :: Line -> Maybe Header
header (LineHeader h) = Just h
header _ = Nothing

comment :: Line -> Maybe Comment
comment (LineComment c) = Just c
comment _ = Nothing

object :: Line -> Maybe Object
object (LineObject o) = Just o
object _ = Nothing

isHeader :: Line -> Bool
isHeader (LineHeader _) = True
isHeader _ = False

isComment :: Line -> Bool
isComment (LineComment _) = True
isComment _ = False

isObject :: Line -> Bool
isObject (LineObject _) = True
isObject _ = False

parseHeaderKey :: Parser Text
parseHeaderKey =
    string "TITLE"
        <|> string "ARTIST"
        <|> string "BPM"
        <|> string "DLEVEL"
        <|> string "DTXVPLAYSPEED"
        <|> string "BGMWAV"
        <|> string "WAV"
        <|> string "VOLUME"
        <|> string "PAN"

-- TODO
parseChannel :: Parser Text
parseChannel = takeTill (== ':')

parseValue :: Parser Text
parseValue = takeTill isEndOfLine

parseHeader :: Parser Header
parseHeader = do
    char '#'
    key <- parseHeaderKey
    chan <- parseChannel
    char ':'
    skipMany space
    value <- parseValue
    Header <$> pure key <*> pure chan <*> pure value

parseObjectKey :: Parser Text
parseObjectKey = do
    n1 <- satisfy $ inClass "0-9A-Z"
    n_ <- count 2 digit
    return $ pack $ n1:n_

parseObject :: Parser Object
parseObject = do
    char '#'
    key <- parseObjectKey
    chan <- parseChannel
    char ':'
    skipMany space
    value <- parseValue
    Object <$> pure key <*> pure chan <*> pure value

parseComment :: Parser Comment
parseComment = do
    _ <- char ';'
    takeTill isEndOfLine

parseBlank :: Parser Comment
parseBlank = endOfLine >>= return ""

parseLine :: Parser Line
parseLine =
    fmap LineComment (parseComment <|> parseBlank)
        <|> fmap LineHeader parseHeader
        <|> fmap LineObject parseObject

parseLines :: Parser [Line]
parseLines = many1 parseLine

readFile :: FilePath -> IO [Line]
readFile fp = runResourceT $ CB.sourceFile fp $$ sinkLines

parseText :: Text -> IO [Line]
parseText t = runResourceT $ CL.sourceList [t] $$ sinkTextLines

sinkLines :: (MonadThrow m) => Sink ByteString m [Line]
sinkLines = decode utf8 =$= sinkParser parseLines

sinkTextLines :: (MonadThrow m) => Sink Text m [Line]
sinkTextLines = sinkParser parseLines
