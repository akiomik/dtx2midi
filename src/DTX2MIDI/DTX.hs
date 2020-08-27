{-# LANGUAGE OverloadedStrings #-}

module DTX2MIDI.DTX
  ( DTX (..),
    Line (..),
    Object (..),
    ObjectValue (..),
    Header (..),
    Key (..),
    Channel (..),
    Comment (..),
    Note (..),
    object,
    header,
    comment,
    isHeader,
    isComment,
    isObject,
    isBPM,
    isBaseBPM,
    isNoteObject,
    headers,
    objects,
    baseBPM,
  )
where

import Data.Maybe (listToMaybe, maybeToList)
import Data.Text (Text, pack, singleton, unpack)

type DTX = [Line]

type Key = Text

type Channel = Text

type Comment = Text

type Note = Text

data Header = Header
  { headerKey :: Key,
    headerChannel :: Channel,
    headerValue :: Text
  }
  deriving (Show, Eq)

-- TODO: Add support for tempo change event and measure length ratio change event
data ObjectValue
  = HiHatClose [Note]
  | Snare [Note]
  | BassDrum [Note]
  | HighTom [Note]
  | LowTom [Note]
  | Cymbal [Note]
  | FloorTom [Note]
  | HiHatOpen [Note]
  | RideCymbal [Note]
  | LeftCymbal [Note]
  | LeftPedal [Note]
  | LeftBassDrum [Note]
  | UnsupportedEvent Channel Text
  deriving (Show, Eq)

data Object = Object
  { objectKey :: Key,
    objectValue :: ObjectValue
  }
  deriving (Show, Eq)

data Line
  = LineHeader Header
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

isBPM :: Header -> Channel -> Bool
isBPM (Header "BPM" c _) c' = c == c'
isBPM _ _ = False

isBaseBPM :: Header -> Bool
isBaseBPM header = isBPM header ""

isNoteObject :: Object -> Bool
isNoteObject (Object _ (UnsupportedEvent _ _)) = False
isNoteObject _ = True

headers :: DTX -> [Header]
headers dtx = (maybeToList . header) =<< dtx

objects :: DTX -> [Object]
objects dtx = (maybeToList . object) =<< dtx

baseBPM :: DTX -> Maybe Double
baseBPM dtx =
  fmap readValue $ listToMaybe $ filter isBaseBPM $ headers dtx
  where
    readValue :: Header -> Double
    readValue = read . unpack . headerValue
