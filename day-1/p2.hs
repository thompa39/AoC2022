module Main (main) where

import           System.Environment
import           System.FilePath
import           System.IO


{-
Not super happy with this solution. Feels like a lot of redundant iterations.
I was also hoping to implement a priority queue, however I ran out of time
to write one in a functional language.
-}


main :: IO ()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let contentLines = lines content
  print $ sum $ topThreeCalories $ asInts $ groupByElf contentLines


groupByElf :: [String] -> [[String]]
groupByElf [] = []
groupByElf ("":strs) = groupByElf strs
groupByElf strs = takeWhile (/= "") strs : groupByElf (dropWhile (/= "") strs)


asInts :: [[String]] -> [[Int]]
asInts = map $ map read


topThreeCalories :: [[Int]] -> [Int]
topThreeCalories = take 3 . revSort . map countElfCalories


countElfCalories :: [Int] -> Int
countElfCalories = sum


revSort :: Ord a => [a] -> [a]
revSort []     = []
revSort (x:xs) = revSort [g | g <- xs, g >= x] ++ [x] ++ revSort [l | l <- xs, l < x]
