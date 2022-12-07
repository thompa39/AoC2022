module Main (main) where

import           System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let contentLines = lines content
  print $ length $ filter isOverlapping contentLines


isOverlapping :: String -> Bool
isOverlapping = processOverlap . splitHalves . split ','


split :: Eq a => a -> [a] -> ([a], [a])
split ch str = (takeWhile (/= ch) str, tail $ dropWhile (/= ch) str)


splitHalves :: (String, String) -> ((Int, Int), (Int, Int))
splitHalves (half1, half2) = ((read n1, read n2), (read n3, read n4))
  where
  (n1, n2) = split '-' half1
  (n3, n4) = split '-' half2


processOverlap :: ((Int, Int), (Int, Int)) -> Bool
processOverlap (range1, range2) = overlap range1 range2 || overlap range2 range1


overlap :: (Int, Int) -> (Int, Int) -> Bool
overlap (l1, u1) (l2, u2) = ((l1 >= l2) && (l1 <= u2)) || ((u1 >= l2) && (u1 <= u2))
