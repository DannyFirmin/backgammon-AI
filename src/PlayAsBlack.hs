module PlayAsBlack where

import State
import PlayAsWhite

makeMove :: State -> Lookahead -> Moves
makeMove s l
  | primesUnder (l * l) < 0 = []
  | otherwise =
        minimaxBot s l
     -- greedyBotV3 s (correspondData s)