module Main (main) where

import           System.Environment
import           System.IO


data Move
  = Rock
  | Paper
  | Scissors
  deriving (Eq)


data Strategy
  = Win
  | Lose
  | Draw


type Match = (Move, Strategy)


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


toStrategy :: String -> Strategy
toStrategy str = case str of
  "X" -> Lose
  "Y" -> Draw
  "Z" -> Win


toMatch :: [String] -> Match
toMatch [x, y] = (toMove x, toStrategy y)


pointsFromMove :: Move -> Int
pointsFromMove move  = case move of
  Rock     -> 1
  Paper    -> 2
  Scissors -> 3


pointsFromPlay :: Strategy -> Int
pointsFromPlay Win  = 6
pointsFromPlay Draw = 3
pointsFromPlay Lose = 0


cycleAmount :: Strategy -> Int
cycleAmount Draw = 0
cycleAmount Win  = 1
cycleAmount Lose = 2


pointsFromMatch :: Match -> Int
pointsFromMatch (them, strat) = (pointsFromMove them - 1 + cycleAmount strat) `mod` 3 + 1 + pointsFromPlay strat
