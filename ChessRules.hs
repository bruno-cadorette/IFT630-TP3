import Data.Map as Map
type Board = Map.Map (Int,Int) ChessPiece

class (Show p)=>Piece p where
    value::p->Int
    movement::p->(Int,Int)->Board->[(Int,Int)]
    
data PieceColor = White | Black deriving(Eq,Show)
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving(Show)

data ChessPiece = ChessPiece {
    pieceType :: PieceType ,
    pieceColor :: PieceColor
} deriving(Show)


pieceValue King = 1000
pieceValue Queen = 9
pieceValue Rook = 5
pieceValue Bishop = 3
pieceValue Knight = 3
pieceValue Pawn = 1

movementImpl (ChessPiece t color) xy game = movementKind xy color game
    where 
        movementKind = 
            case t of
                King -> movementKing
                Queen -> movementQueen
                Rook -> movementRook
                Bishop -> movementBishop
                Knight -> movementKnight
                Pawn -> movementPawn
{-
instance Piece ChessPiece where
    value (ChessPiece t _)= pieceValue t
    movement = movementImpl
    -}
borderMin = max 0
borderMax = min 7

block color board = block' 
    where 
        block' [] = []
        block' (pos:xs) =
            case Map.lookup pos board of
                Just(x)->
                    if pieceColor x /= color then
                        [pos] -- on mange la piece adverse
                    else 
                        []
                Nothing->pos:block' xs

                
movementRook (x,y) c g = up++down ++right++left
    where
        up = block c g [(x,i)|i<-[borderMax (y+1) .. borderMax (y+7)]]
        down = block c g [(x,i)|i<-[borderMin (y-1), borderMin (y-2) .. borderMin (y-7)]]
        right = block c g [(i,y)|i<-[borderMax (x+1) .. borderMax (x+7)]]
        left = block c g [(i,y)|i<-[borderMin (x-1), borderMin (x-2) .. borderMin (x-7)]]

{-
    concatMap (\(d, (axe, getPos))->block c g [getPos i|i<-[(axe+(d*1))..(borderMax axe + (d * 8))]]) params
    where 
        params = do
            directions <- [-1,1] 
            getPos <- [(y, (\i->(x,i))),(x,(\i->(i,y)))]
            return (directions, getPos)-}

movementKing (x,y) c g = []--[(i,j)|i<-(block c g) [(borderMin x-1)..(borderMax x+1)], j<-[(borderMin y-1)..(borderMax y+1)],i/=j]
--movementRook (x,y)c g = concat [[(i,y)|i<-(block c g) [(borderMin x-8)..(borderMax x+8)],i/=x], [(x,i)|i<-(block c g)[(borderMin y-8)..(borderMax y+8)],i/=y]]
movementBishop (x,y) c g = []
movementKnight a c g = []
movementQueen pos c g = []--concat [movementRook pos c g, movementBishop pos c g]
movementPawn a b c = []
{-
movementPawn White (x,y) game = canEat (x+1,y+1) White [(x-1,y+1)] ++ (if y == 1 then [(x,y+2)] else [])
    where
        canEat pos color = 
            case Map.lookup pos game of
                Just (ChessPiece t c) -> c /= color
                Nothing ->[]
        
movementPawn Black (x,y) game = 
    if (y == 6) then [(x,y-2)]
    else [(x,y-1)]
-}