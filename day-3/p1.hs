module Main (main) where

import           Data.Char          (isUpper, ord)
import qualified Data.Set           as Set
import           System.Environment (getArgs)
import           System.IO          ()


main :: IO ()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let contentLines = lines content
  print $ sum $ map processBag contentLines


halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) (xs)


processBag :: String -> Int
processBag bag = sum $ map getPriority $ Set.toList $ Set.intersection (comp1) (comp2)
  where
    (half1, half2) = halve bag
    comp1 = Set.fromList half1
    comp2 = Set.fromList half2


getPriority :: Char -> Int
getPriority ch = if isUpper ch then asInt - 38 else asInt - 96
  where
    asInt = ord ch
