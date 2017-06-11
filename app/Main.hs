module Main where

import Lib
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [file1, file2] -> putStr . getDiff =<< zipFileLines file1 file2
    _ -> putStrLn "Please provide two file paths."
