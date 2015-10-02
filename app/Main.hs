module Main where

import System.Environment (getArgs)

-- import Lib
import DtxParser

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  putStrLn $ "Input file: " ++ file
  dtx <- readFile file
  putStrLn $ "File content: " ++ dtx
  test dtx
