module PlayAsBlack where

import State
import Move
import Data.List
import Data.Tree
import Board
import Player
-- import Debug.Trace (trace,traceShowId)

makeMove :: State -> Lookahead -> Moves
makeMove s@(State st (Board pt wb bb) tn ml wp bp ws bs) l
  | primesUnder (l * l) < 0 = []
--   | (wb - bp) >= 40 = minimaxBotV2 s l
  | otherwise = greedyBotV3 s (correspondData s)
     --greedyBotV1 s movesLeft

legalMoveBot::State ->  (State -> [Int]) -> Moves
legalMoveBot s f =
  case legalMoves s of
      x:xs -> head (legalMoves s):legalMoveBot s' f
      [] -> []
   where
   s'::State
   s' = performSingleMove s (head (legalMoves s))


greedyBotV1 :: State -> (State -> [Int]) -> Moves
greedyBotV1 s f =
 case legalMoves s of
    x:xs -> greedyHeuristics s:greedyBotV1 s' f
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

greedyBotV3 :: State -> ([Int] ,[Move]) -> Moves
greedyBotV3 s corres = case corres of
     ([], [])     -> []
     (score, move) -> (move !! bestPositionIndex score 0) : greedyBotV3 s' corres'

     where
            s' :: State
            s' = performSingleMove s (legalMoves s !! bestPositionIndex (scoreEachState s) 0)
            corres' = correspondData s'

minimaxBotV1 :: State -> Lookahead -> Moves
minimaxBotV1 s l =
 case legalMoves s of
    x:xs -> rootV1 s l: minimaxBotV1 s' l
    [] -> []

    where
    s'::State
    s' = performSingleMove s (rootV1 s l)

minimaxBotV2 :: State -> Lookahead -> Moves
minimaxBotV2 s l =
 case legalMoves s of
    x:xs -> rootV2 s l: minimaxBotV2 s' l
    [] -> []

    where
    s'::State
    s' = performSingleMove s (rootV2 s l)

bestPositionIndex :: [Int] -> Int -> Int
bestPositionIndex list index = case list of
   x:xs | x == maximum list -> index
        | otherwise   -> bestPositionIndex xs (index + 1)
   [] -> 0

correspondData :: State -> ([Int],[Move])
correspondData s = (scoreEachState s , legalMoves s)

-- bestMove2 :: ([Int],[Move]) -> Move
-- bestMove2 (i,m) = (elemIndices (maximum i) i)


scoreEachState :: State -> [Int]
scoreEachState s = map scoreState (listofNewState s)


--This is the heuristics for my greedyBotV1,
-- using my creative, simple but effective idea different from the evaluate idea that all the students have
-- This is the heuristic only for the greedyBotV1
greedyHeuristics :: State -> Move
greedyHeuristics s@(State st (Board pt wb bb) tn ml wp bp ws bs)
 |block pt /= [6611178] && goToPoint (block pt) (legalMoves s) /= (6611178,6611178) = goToPoint (block pt) (legalMoves s)
 |blot pt /= [6611178] && goToPoint (blot pt) (legalMoves s) /= (6611178,6611178) = goToPoint (blot pt) (legalMoves s)
 |ml /= [] && isLegalMove s (findFurthest pt,head ml)= (findFurthest pt,head ml)
 |otherwise =  bestMove(listofMove s (legalMoves s))

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
-- This is one of the important function of my greedyV1 ideas
goToPoint :: [Int] -> Moves -> Move
goToPoint p (x:xs)
--  |fst x - snd x == p  = x
--  |fst x - snd x /= p = goToPoint p xs
 |(fst x - snd x) `elem` p = x
 |(fst x - snd x) `notElem` p = goToPoint p xs
 |otherwise = (6611178,6611178)
goToPoint _ _ = (6611178,6611178)

--a position that can bolt black

block :: [Point] -> [Int]
block pt
  |Just(White,1) `elem` pt = map (+1)  (elemIndices (Just(White,1)) pt)
  |otherwise = [6611178]

blot :: [Point] -> [Int]
blot pt
  |Just(Black,1) `elem` pt = map (+1) (elemIndices (Just(Black,1)) pt)
  |otherwise = [6611178]


--greedyBotV1 ends here. And greedyBotV2 that using scoring starts here

scoreBeenEaten :: State -> Int
scoreBeenEaten s@(State st (Board pt wb bb) tn ml wp bp ws bs)
 |wb > 0 = -100
 |otherwise = 0

scoreEat :: State -> Int
scoreEat s@(State st (Board pt wb bb) tn ml wp bp ws bs)
 |bb > 0 = 100
 |otherwise = 0


scoreGoodMove :: State -> Int
scoreGoodMove s@(State st (Board pt wb bb) tn ml wp bp ws bs)
 |bp - wp >= 24 = 50
 |otherwise = 0

scorePoint :: [Point] -> Int
scorePoint (p:ps) = case p of
  Just(Black,1) -> 10 + scorePoint ps
  Just(White,1) -> -300 + scorePoint ps
  Just(White,2) -> 3 + scorePoint ps
  Just(White,n)
            |n>=7 -> 1 + scorePoint ps

  _ -> 0
scorePoint [] = 0


-- This is actually the main heuristics for greedyBotV2, minimax and a-b punning.
scoreState :: State -> Int
scoreState s@(State st (Board pt wb bb) tn ml wp bp ws bs) = -((bp - wp)*2 + scoreBeenEaten s + scoreEat s + scoreGoodMove s + scorePoint pt)


listofNewState :: State -> [State]
listofNewState s = map (performSingleMove s) (legalMoves s)


listofMove :: State -> Moves -> [(Int,Move)]
listofMove s@(State st (Board pt wb bb) tn ml wp bp ws bs) (m:ms) = (scoreState (performSingleMove s m),m):listofMove s ms
listofMove s _ = [(0,head (legalMoves s))]



bestMove :: [(Int,Move)] -> Move
bestMove bm = snd(maximum bm)

combine :: State -> Move
combine s = bestMove(listofMove s (legalMoves s))



--Minimax bot starts here. Inspired by Tony's lecture and his lecture code
roseTree :: (a -> [a]) -> a -> Tree a
roseTree fun a = Node a (map (roseTree fun) (fun a))

treeElem :: State -> Moves -> [State]
treeElem s (m:ms) =  performSingleMove s m:treeElem s ms


gameTree :: State -> Tree State
gameTree s = Node s (map (legalMovesTree.performSingleMove s) (legalMoves s))

pruning :: Int -> Tree a -> Tree a
pruning _ (Node a []) = Node a []
pruning 0 (Node a list) = Node a []
pruning n (Node a list) = Node a (map (pruning (n - 1)) list)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap fun rt = case rt of
  Node a [] -> Node (fun a) []
  Node a r -> Node (fun a) (map(treeMap fun) r)

maximise :: (Ord a) => Tree a -> a
maximise (Node a []) = a
maximise (Node a sub) = maximum (map minimise sub)

minimise :: (Ord a) => Tree a -> a
minimise (Node a []) = a
minimise (Node a sub) = minimum (map maximise sub)

minimaxV1 :: Lookahead -> State -> Int
minimaxV1 lh = maximise . treeMap scoreState. pruning lh . gameTree

primesUnder :: Int -> Int
primesUnder n = length $ filterPrime [2 .. n]
  where
    filterPrime [] = []
    filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]


rootV1 :: State -> Lookahead -> Move
rootV1 s n = snd $ maximum results
  where
  results = zip (map (minimaxV1 (n * 2)) (listofNewState s)) (legalMoves s)


rootV2 :: State -> Lookahead -> Move
rootV2 s n = snd $ maximum results
  where
  results = zip (map (minimaxV2 (n * 2)) (listofNewState s)) (legalMoves s)


minimaxV2 :: Lookahead -> State -> Int
minimaxV2 0 s = scoreState s
minimaxV2 n s =
    case turn s of
        White -> case map (minimaxV2 (n-1)) (listofNewState s) of
            [] -> 0
            _ -> maximum (map (minimaxV2 (n-1)) (listofNewState s))

        Black -> case map (minimaxV2 (n-1)) (listofNewState s) of
            [] -> 0
            _ -> minimum (map (minimaxV2 (n-1)) (listofNewState s))