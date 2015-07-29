module AlgebraicChessNotation (getMoveInput)  where

import Data.List
import Data.Char
import Data.Maybe
import Data.List.Split

getCoordonate [column,row] =
  (fromJust $ column `elemIndex` ['a'..'z'], digitToInt (row) - 1)

getMoveInput :: String -> ((Int,Int),(Int,Int))
getMoveInput input = toTuple coordonates
    where
        coordonates = map getCoordonate $ splitOn "-"  input
        toTuple [x,y] = (x,y)
        