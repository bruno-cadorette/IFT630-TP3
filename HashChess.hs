import Tp3.Chess
import Data.Map


charPiece White Pawn = 1
charPiece Black Pawn = 2
charPiece White Knight = 3
charPiece Black Knight= 4
charPiece White Bishop = 5
charPiece Black Bishop = 6
charPiece White Rook  = 7
charPiece Black Rook  = 8
charPiece White Queen = 9
charPiece Black Queen = 10
charPiece White King= 11
charPiece Black King = 12
charPiece _ = 0

hashChess plateau =
  Data.Map.foldl (\ acc y -> acc ++  show y) "" plateau
