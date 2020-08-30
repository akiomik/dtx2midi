module Main where

import Codec.Text.Detect (detectEncodingName)
import DTX2MIDI (dtxToMIDI, fromDTXFile, toMIDIFile)
import qualified Data.ByteString.Lazy as B
import Data.Semigroup ((<>))
import Data.Version (showVersion)
import Options.Applicative
import qualified Paths_dtx2midi

data Opts = Opts
  { input :: FilePath,
    output :: FilePath
  }

opts :: Parser Opts
opts =
  Opts
    <$> strOption
      ( long "input"
          <> short 'i'
          <> metavar "DTXFILE"
          <> help "An input .dtx file"
      )
    <*> strOption
      ( long "output"
          <> short 'o'
          <> metavar "MIDIFILE"
          <> help "An output .midi file"
      )

run :: Opts -> IO ()
run (Opts input output) = do
  putStrLn $ "[INFO] Input file: " ++ input
  putStrLn $ "[INFO] Output file: " ++ output
  checkInputEncoding input
  dtx <- fromDTXFile input
  midi <- dtxToMIDI dtx
  toMIDIFile output midi
  putStrLn "[INFO] Complete!"

checkInputEncoding :: FilePath -> IO ()
checkInputEncoding fp = do
  file <- B.readFile fp
  case detectEncodingName file of
    Just enc | enc `elem` ["ASCII", "UTF-8", "UTF-16BE", "UTF-16LE", "UTF-32BE", "UTF-32LE"] -> do
      return ()
    Just enc -> do
      putStrLn $ "[WARN] The conversion may fail due to an unsupported encoding '" ++ enc ++ "' being detected in " ++ fp ++ "."
    Nothing -> do
      putStrLn $ "[WARN] The conversion may fail due to an unknown encoding being detected in " ++ fp ++ "."

version :: String
version = showVersion Paths_dtx2midi.version

main :: IO ()
main = run =<< execParser args
  where
    args =
      info
        (opts <**> helper)
        ( fullDesc
            <> progDesc "Convert .dtx file into .midi file"
            <> header ("dtx2midi " ++ version ++ " - .dtx -> .midi converter")
        )
