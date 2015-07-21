import Data.Map

charPiece empty = 0
charPiece wPawns = 1
charPiece bPawns = 2
charPiece wKnights = 3
charPiece bKnights= 4
charPiece wBishops = 5
charPiece bBishops = 6
charPiece wRooks  = 7
charPiece bRooks  = 8
charPiece wQueen = 9
charPiece bQueen = 10
charPiece wKing= 11
charPiece bKing = 12

hashChess plateau =
  Data.Map.foldl (\ acc y -> acc ++  show y) "" plateau
