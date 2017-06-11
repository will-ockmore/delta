module Lib
    ( zipFileLines
    , getDiff
    ) where

import Control.Monad
import Control.Applicative

zipFileLines :: FilePath -> FilePath -> IO [(String, String)]
zipFileLines a b = liftA2 zip (getLines a) (getLines b)
                    where
                      getLines fp = readFile fp >>= (return . lines)

findLine :: String -> [(String, String)] -> String
findLine b xs =
  let
    ys = dropWhile (\(_, r) -> r /= b) xs
    zs = takeWhile (\(a, _) -> a /= b) ys
  in case (ys == zs) of
    -- Do we find the line in the old file?
    -- If not then ys == zs
    True -> "+ " ++ b
    False -> unlines . fmap (\(l, _) -> "- " ++ l) $ init zs

getDiff :: [(String, String)] -> String
getDiff xs = unlines $ fmap diff xs
             where
              diff (a, b) = case (a == b) of {True -> a; False -> findLine b xs;}
