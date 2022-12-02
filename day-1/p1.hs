module Main (main) where

import           System.Environment
import           System.FilePath
import           System.IO


main :: IO ()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let contentLines = lines content
  print $ maxCalories $ asInts $ groupByElf contentLines


groupByElf :: [String] -> [[String]]
groupByElf [] = []
groupByElf ("":strs) = groupByElf strs
groupByElf strs = takeWhile (/= "") strs : groupByElf (dropWhile (/= "") strs)


asInts :: [[String]] -> [[Int]]
asInts = map $ map read


maxCalories :: [[Int]] -> Int
maxCalories = maximum . map countElfCalories


countElfCalories :: [Int] -> Int
countElfCalories = sum
