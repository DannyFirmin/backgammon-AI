module PlayAsBlack where

import State
import Move
import Data.List
import Board
import Player
--import Debug.Trace

makeMove :: State -> Lookahead -> Moves
makeMove s l
  | primesUnder (l * l) < 0 = []
  | otherwise =
      greedyBot s movesLeft

legalMoveBot::State ->  (State -> [Int]) -> Moves
legalMoveBot s f =
  case legalMoves s of
      x:xs -> head (legalMoves s):legalMoveBot s' f
      [] -> []
   where
   s'::State
   s' = performSingleMove s (head (legalMoves s))


greedyBot :: State -> (State -> [Int]) -> Moves
greedyBot s f =
 case legalMoves s of
    x:xs -> greedyHeuristics s:greedyBot s' f
    [] -> []

    where
    s'::State
    s' = performSingleMove s (greedyHeuristics s)

greedyHeuristics :: State -> Move
greedyHeuristics s@(State st (Board pt wb bb) tn ml wp bp ws bs)
 |ml /= [] && isLegalMove s (findFurthest pt,head ml)= (findFurthest pt,head ml)
 |otherwise =  head (legalMoves s)

findFurthest :: [Point] -> Int
findFurthest pt =
 case elemIndices White (fst(unzip(reverse (cancelMaybe  pt)))) of
  x:xs -> 24 - head(elemIndices White (fst(unzip(reverse (cancelMaybe  pt)))))
  [] -> 24

cancelMaybe::[Point]->[(Player, Int)]
cancelMaybe list = case list of
 [] -> []
 (Nothing:xs) -> (Black,0):cancelMaybe xs
 (Just x:xs) -> x:cancelMaybe xs


primesUnder :: Int -> Int
primesUnder n = length $ filterPrime [2 .. n]
  where
    filterPrime [] = []
    filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]
