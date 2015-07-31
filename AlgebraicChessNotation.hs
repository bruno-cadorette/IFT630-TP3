module AlgebraicChessNotation (getMoveInput, getNotation)  where

import Data.List
import Data.Char
import Data.Maybe
import Data.List.Split
import ChessPiece
verif (x, y)
    |x >= 8 || y >= 8 || x < 0 || y < 0 = error  $ "La valeur " ++ show (x,y) ++ " n'est pas valide"
    |otherwise = (x,y)
    
getCoordonate :: String -> Position
getCoordonate [column,row] = verif (x, y)
    where 
        x = (ord column - ord 'a')
        y = digitToInt row - 1
        
  
getNotation :: Position -> Position -> String
getNotation origin target = positionToChess origin ++ "-" ++ positionToChess target

positionToChess :: Position -> String
positionToChess (x,y) = [chr $ ord  'a' + x] ++ show (y + 1)

getMoveInput :: String -> (Position, Position)
getMoveInput input = (\(a,b)->(verif a, verif b)) $ toTuple coordonates
    where
        coordonates = map getCoordonate $ splitOn "-"  input
        toTuple [x,y] = (x,y)
        