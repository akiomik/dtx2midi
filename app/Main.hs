module Main where

import Data.Semigroup ((<>))
import Data.Version (showVersion)
import Options.Applicative

import qualified Paths_dtx2midi
import DTX2MIDI (fromFile, toFile)

data Opts = Opts
    { input :: String
    , output :: String }

opts :: Parser Opts
opts = Opts
    <$> strOption
        ( long "input"
       <> short 'i'
       <> metavar "DTXFILE"
       <> help "An input .dtx file" )
    <*> strOption
        ( long "output"
       <> short 'o'
       <> metavar "MIDIFILE"
       <> help "An output .midi file" )

run :: Opts -> IO ()
run (Opts input output) = do
    putStrLn $ "Input file: " ++ input
    putStrLn $ "Output file: " ++ output
    midi <- fromFile input
    toFile output midi
    putStrLn "Complete!"

version :: String
version = showVersion Paths_dtx2midi.version

main :: IO ()
main = run =<< execParser args
  where
    args = info (opts <**> helper)
      ( fullDesc
     <> progDesc "Convert .dtx file into .midi file"
     <> header ("dtx2midi " ++ version ++ " - .dtx -> .midi converter") )
