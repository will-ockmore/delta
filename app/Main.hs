module Main where

import Lib
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [file1, file2] -> do
        f1 <- getLines file1
        f2 <- getLines file2
        putStr $ getDiff f1 f2
    _ -> putStrLn "Please provide two file paths."
  where
    getLines fp = readFile fp >>= (return . lines)
