module Main where

import System.Environment (getArgs)
import Haskore.Interface.MIDI.Render (playTimidity)

import DTX2MIDI

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  putStrLn $ "Input file: " ++ file
  midi <- fromFile file
  _ <- playTimidity midi
  return ()

