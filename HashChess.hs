import Tp3.Chess
import Data.Map

charPiece color pieceType =   2 * fromEnum pieceType + fromEnum color

hashChess plateau =
  Data.Map.foldl (\ acc y -> acc ++  show y) "" plateau
