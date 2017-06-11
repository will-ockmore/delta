module Lib
    ( zipFileLines
    , getDiff
    ) where

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Applicative

zipWithPadding :: (Monoid a, Monoid b) => [a] -> [b] -> [(a, b)]
zipWithPadding (x:xs) (y:ys) = (x, y) : zipWithPadding xs ys
zipWithPadding xs [] = zip xs (repeat mempty)
zipWithPadding [] ys = zip (repeat mempty) ys

zipFileLines :: FilePath -> FilePath -> IO [(String, String)]
zipFileLines a b = liftA2 zipWithPadding (getLines a) (getLines b)
                    where
                      getLines fp = readFile fp >>= (return . lines)

getRawDiff :: [String] -> [String] -> [String] -> [String]
getRawDiff rs (x:xs) (y:ys)
    | x == y    = getRawDiff (y:rs) xs ys
    | otherwise = composeChunk [] (x:xs) (y:ys)
    where
      composeChunk _ [] (u:us) = getRawDiff (("+ " ++ u):rs) xs ys
      composeChunk res (t:ts) (u:us)
          | t == u    = getRawDiff (u:res ++ rs) ts us
          | otherwise = composeChunk (("- " ++ t) : res) ts (u:us)

getRawDiff rs [] ys = (fmap ("- " ++) ys) ++ rs
getRawDiff rs _ []  = rs

getDiff :: [String] -> [String] -> String
getDiff xs ys = unlines . reverse $ getRawDiff [] xs ys


















