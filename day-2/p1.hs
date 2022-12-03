module Main (main) where

import           System.Environment
import           System.IO


data Move
  = Rock
  | Paper
  | Scissors
  deriving (Eq)

type Match = (Move, Move)


main :: IO ()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let contentLines = lines content
  print $ sum $ map pointsFromMatch $ toMatches contentLines


toMatches :: [String] -> [Match]
toMatches = map toMatch . map words


toMove :: String -> Move
toMove str = case str of
  "A" -> Rock
  "B" -> Paper
  "C" -> Scissors
  "X" -> Rock
  "Y" -> Paper
  "Z" -> Scissors


toMatch :: [String] -> Match
toMatch [x, y] = (toMove x, toMove y)


pointsFromMove :: Move -> Int
pointsFromMove move = case move of
  Rock     -> 1
  Paper    -> 2
  Scissors -> 3


pointsFromPlay :: Match -> Int
pointsFromPlay (Rock, Paper)     = 6
pointsFromPlay (Scissors, Rock)  = 6
pointsFromPlay (Paper, Scissors) = 6
pointsFromPlay (x, y)            = if x == y then 3 else 0


pointsFromMatch :: Match -> Int
pointsFromMatch (them, us) = pointsFromMove us + pointsFromPlay (them, us)
