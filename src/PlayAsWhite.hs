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
      greedyBotV2 s movesLeft

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

greedyBotV2 :: State -> (State -> [Int]) -> Moves
greedyBotV2 s f =
 case legalMoves s of
    x:xs -> combine s:greedyBotV2 s' f
    [] -> []

    where
    s'::State
    s' = performSingleMove s (combine s)

greedyHeuristics :: State -> Move
greedyHeuristics s@(State st (Board pt wb bb) tn ml wp bp ws bs)
 |block pt /= [6611178] && goToPoint (block pt) (legalMoves s) /= (6611178,6611178) = goToPoint (block pt) (legalMoves s)
 |blot pt /= [6611178] && goToPoint (blot pt) (legalMoves s) /= (6611178,6611178) = goToPoint (blot pt) (legalMoves s)
 |ml /= [] && isLegalMove s (findFurthest pt,head ml)= (findFurthest pt,head ml)
 |otherwise =  head (legalMoves s)

findFurthest :: [Point] -> Int
findFurthest pt =
 case elemIndices White (map fst(reverse (cancelMaybe pt))) of

  x:xs -> 24 - head(elemIndices White (map fst(reverse (cancelMaybe pt))))
  [] -> 24

cancelMaybe::[Point]->[(Player, Int)]
cancelMaybe list = case list of
 [] -> []
 (Nothing:xs) -> (Black,0):cancelMaybe xs
 (Just x:xs) -> x:cancelMaybe xs


--give a position you want to go, then give the move that you can go there (14,4)
goToPoint :: [Int] -> Moves -> Move
goToPoint p (x:xs)
--  |fst x - snd x == p  = x
--  |fst x - snd x /= p = goToPoint p xs
 |(fst x - snd x) `elem` p = x
 |(fst x - snd x) `notElem` p = goToPoint p xs
-- |[(a, b) | a <- (p:ps), b <- (x:xs), fst x - snd x == p]
 |otherwise = (6611178,6611178)
goToPoint _ _ = (6611178,6611178)

--a position that can bolt black

-- (a,b)
-- |fst(a,b)-snd(a,b)==9 = (a,b)
block :: [Point] -> [Int]
block pt
  |Just(White,1) `elem` pt = map (+1)  (elemIndices (Just(White,1)) pt)
  |otherwise = [6611178]

blot :: [Point] -> [Int]
blot pt
  |Just(Black,1) `elem` pt = map (+1) (elemIndices (Just(Black,1)) pt)
  |otherwise = [6611178]




--[(heuristic score, move)]
scoreState :: State -> Int
scoreState s@(State st (Board pt wb bb) tn ml wp bp ws bs)
  |wb > 0 = -100
  |bb > 0 = 50
  |bp - wp >= 24 = 100
  |Just(White,1) `elem` pt = -50
  |otherwise = 0

listofMove :: State -> Moves -> [(Int,Move)]
listofMove s@(State st (Board pt wb bb) tn ml wp bp ws bs) (m:ms) = (scoreState (performSingleMove s m),m):listofMove s ms
listofMove s _ = [(0,head (legalMoves s))]

bestMove :: [(Int,Move)] -> Move
bestMove bm = snd(maximum bm)

combine :: State -> Move
combine s = bestMove(listofMove s (legalMoves s))


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