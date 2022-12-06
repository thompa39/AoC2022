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
  print $ sum $ map processGroup  $ intoThrees contentLines


processGroup :: [String] -> Int
processGroup = sum . map getPriority . Set.toList . foldl1 Set.intersection . map Set.fromList


intoThrees :: [a] -> [[a]]
intoThrees []         = []
intoThrees (x:y:z:xs) = [x, y, z]:intoThrees xs


getPriority :: Char -> Int
getPriority ch = if isUpper ch then asInt - 38 else asInt - 96
  where
    asInt = ord ch
