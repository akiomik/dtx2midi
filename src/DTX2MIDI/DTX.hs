{-# LANGUAGE OverloadedStrings #-}

module DTX2MIDI.DTX
  ( Line (..),
    Object (..),
    Header (..),
    Comment (..),
    Note (..),
    Key (..),
    object,
    header,
    comment,
    isHeader,
    isComment,
    isObject,
  )
where

import Data.Text (Text, pack, singleton)

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

data Object = Object
  { objectKey :: Key,
    objectChannel :: Channel,
    objectValue :: [Note]
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
