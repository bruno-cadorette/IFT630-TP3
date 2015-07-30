module AlgebraicChessNotation (getMoveInput, getNotation)  where

import Data.List
import Data.Char
import Data.Maybe
import Data.List.Split
import ChessPiece

getCoordonate :: String -> Position
getCoordonate [column,row] = ((ord column - ord 'a'), digitToInt row - 1)
  
getNotation :: Position -> Position -> String
getNotation origin target = positionToChess origin ++ "-" ++ positionToChess target

positionToChess :: Position -> String
positionToChess (x,y) = [chr $ ord  'a' + x] ++ show (y + 1)

getMoveInput :: String -> (Position, Position)
getMoveInput input = toTuple coordonates
    where
        coordonates = map getCoordonate $ splitOn "-"  input
        toTuple [x,y] = (x,y)
        