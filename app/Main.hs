module Main where

import System.Environment (getArgs)
import Haskore.Interface.MIDI.Render (playTimidity)

import DTX2MIDI

main :: IO ()
main = do
    args <- getArgs
    let input = head args
    let output = args !! 1
    putStrLn $ "Input file: " ++ input
    putStrLn $ "Output file: " ++ output
    midi <- fromFile input
    toFile output midi
    putStrLn "Complete!"
