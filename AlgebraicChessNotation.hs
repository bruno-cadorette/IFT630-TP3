module AlgebraicChessNotation (getMoveInput)  where

import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Text as T

getCoordonate [column,row] =
  (fromJust $ column `elemIndex` ['a'..'z'], digitToInt (row) - 1)

getMoveInput :: String -> ((Int,Int),(Int,Int))
getMoveInput input = toTuple coordonates
    where
        coordonates = map (getCoordonate . T.unpack) $ T.splitOn (T.pack "-") $ T.pack input
        toTuple [x,y] = (x,y)
        