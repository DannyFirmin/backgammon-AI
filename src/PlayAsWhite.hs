module PlayAsWhite where

import State
import Move
import Data.List
import Board
import Player
--import Debug.Trace (trace,traceShowId)

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
 |ml /= [] && isLegalMove s ((findFurthest pt),head ml)= ((findFurthest pt),(head ml))
 |otherwise =  head (legalMoves s)

findFurthest :: [Point] -> Int
findFurthest pt =
 case elemIndices White (fst(unzip(reverse (convertertt  pt)))) of
  x:xs -> 24 - head(elemIndices White (fst(unzip(reverse (convertertt  pt)))))
  [] -> 24

convertertt::[Point]->[(Player, Int)]
convertertt list = case list of
 [] -> []
 (Nothing:xs) -> (Black,0):convertertt xs
 (Just x:xs) -> x:convertertt xs

scoreState :: State -> Int
scoreState s@(State _ (Board pt wb bb) tn ml wp bp ws bs) =
   case bb of
    0 -> 0
    _ -> 100

   --if b@(Board pt wb bb)

stateCompare :: State -> State -> State
stateCompare s1 s2
  |scoreState s1 >= scoreState s2 = s1
  |scoreState s1 < scoreState s2 = s2
  |otherwise = s1

--  |(Black,1) `elem` pt = (Black,1)!!pt
--   |stateCompare s1 s2 == s1 = head(legalMoves s)
--
--   where
--   s1::State
--   s1= performSingleMove s (head(legalMoves s))
--
--   s2::State
--   s2= performSingleMove s (last(legalMoves s))
primesUnder :: Int -> Int
primesUnder n = length $ filterPrime [2 .. n]
  where
    filterPrime [] = []
    filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]
