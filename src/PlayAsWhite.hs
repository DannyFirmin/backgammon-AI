module PlayAsWhite where

import State
import Move

-- import Data.List
-- import Debug.Trace (trace)
makeMove :: State -> Lookahead -> Moves
makeMove s l
  | primesUnder (l * l) < 0 = []
  | otherwise =
      legalMoveBot s

-- get ideas from tutor and friend
legalMoveBot::State -> Moves
legalMoveBot s@(State _ _ _ ml _ _ _ _) =
  case ml of
   x:xs -> case legalMoves s of
      y:ys -> head (legalMoves s):legalMoveBot s'
      [] -> []
   [] ->[]

   where
   s'::State
   s' = performSingleMove s (head (legalMoves s))


primesUnder :: Int -> Int
primesUnder n = length $ filterPrime [2 .. n]
  where
    filterPrime [] = []
    filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]
