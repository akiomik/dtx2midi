module DtxParser
    (
    parser,
    test
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)

data Line = Header String String | Object String String String | Comment deriving Show

upperNum :: Parser Char
upperNum = do
  digit <|> upper

-- parse channel
channel :: Parser String
channel = do
  count 2 upperNum

-- WAV00のようなチャンネル指定するタイプのヘッダー
channelHeaderKey :: String -> Parser String
channelHeaderKey s = do
  string s
  c <- channel
  return $ s ++ c

headerKeys :: Parser String
headerKeys = do
  try (string "TITLE")
      <|> try (string "ARTIST")
      <|> try (string "BPM")
      <|> try (string "DLEVEL")
      <|> try (string "DTXVPLAYSPEED")
      <|> try (string "BGMWAV")
      <|> try (channelHeaderKey "WAV")
      <|> try (channelHeaderKey "VOLUME")
      <|> try (channelHeaderKey "PAN")

-- parse header
-- eg. "TITLE: Story of Hope"
header :: Parser DtxParser.Line
header = do
  char '#'
  key <- headerKeys
  char ':'
  optional spaces
  value <- many $ noneOf ";\n\r"
  return $ Header key value

-- parse measure
-- eg. "000", "Z99"
measure :: Parser String
measure = do
  n1 <- upperNum
  n2 <- count 2 digit
  return $ [n1] ++ n2

-- parse objct data
obj :: Parser String
obj = do
  skipMany $ char '_'
  count 2 upperNum

-- parse object
-- eg. "00111: 0101010101010101"
object :: Parser DtxParser.Line
object = do
  char '#'
  m <- measure
  c <- channel
  char ':'
  optional space
  os <- many obj
  let o = concat os
  return $ Object m c o

-- parse comment
-- eg. "; This is comment"
comment :: Parser DtxParser.Line
comment = do
  (do {char ';'; many $ noneOf "\r\n"})
  return Comment

-- parse line
line :: Parser DtxParser.Line
line = do
  optional $ many endOfLine
  l <- (try header) <|> (try object) <|> comment
  optional $ many endOfLine
  return l

-- parse dtx
parser :: Parser [DtxParser.Line]
parser = many line

test :: String -> IO ()
test s = parseTest parser s
