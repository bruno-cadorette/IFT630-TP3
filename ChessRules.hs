import Data.Map as Map
type Board = Map.Map (Int,Int) ChessPiece

class (Show p)=>Piece p where
    value::p->Int
    movement::p->(Int,Int)->[(Int,Int)]
    
data PieceColor = White | Black deriving(Eq,Show)
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving(Show)

data ChessPiece = ChessPiece PieceType PieceColor deriving(Show)


pieceValue King = 1000
pieceValue Queen = 9
pieceValue Rook = 5
pieceValue Bishop = 3
pieceValue Knight = 3
pieceValue Pawn = 1

movementImpl (ChessPiece t color) xy game = (movementKind xy) color game
    where 
        movementKind = 
            case t of
                King -> movementKing
                Queen -> movementQueen
                Rook -> movementRook
                Bishop -> movementBishop
                Knight -> movementKnight
                Pawn -> movementPawn

instance Piece ChessPiece where
    value (ChessPiece t _)= pieceValue t
    movement = movementImpl
    
borderMin = min 0
borderMax = max 8

movementKing (x,y) = [(i,j)|i<-[(borderMin x-1)..(borderMax x+1)], j<-[(borderMin y-1)..(borderMax y+1)],i/=j]
movementRook (x,y) =concat [[(i,y)|i<-[(borderMin x-8)..(borderMax x+8)],i/=x], [(x,i)|i<-[(borderMin y-8)..(borderMax y+8)],i/=y]]
movementBishop (x,y) = []
movementKnight a= []
movementQueen pos = concat [movementRook pos, movementBishop pos]


movementPawn White (x,y) game = canEat (x+1,y+1) White [(x-1,y+1)] ++ (if y == 1 then [(x,y+2)] else [])
    where
        canEat pos color = 
            case Map.lookup pos game of
                Just (ChessPiece t c) -> c /= color
                Nothing ->[]
        
movementPawn Black (x,y) game = 
    if (y == 6) then [(x,y-2)]
    else [(x,y-1)]
