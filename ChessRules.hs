import qualified Data.Map as Map
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

isOutOfMap (x,y) = x >= 8 || y >= 8 || x < 0 || y < 0

block color board = block' 
    where 
        block' [] = []
        block' (pos:xs)
            |isOutOfMap pos = []
            |otherwise = 
                case Map.lookup pos board of
                    Just(x)->
                        if pieceColor x /= color then
                            [pos] -- on mange la piece adverse
                        else 
                            []
                    Nothing->pos:block' xs

movementKing (x,y) c g = filter (\(i,j)-> (not (isOutOfMap (i,j))) && (not (x==i && y == j)) && canEat (i,j) c g) possibleMovements
    where
        possibleMovements = 
            [(i,j)|i<-[(borderMin x-1)..(borderMax x+1)], j<-[(borderMin y-1)..(borderMax y+1)]]
        canEat pos myColor board = 
            case Map.lookup pos board of 
                Just(x) -> pieceColor x /= myColor
                Nothing -> True
        
movementQueen pos c g = concatMap (\f-> f pos c g) [movementRook, movementBishop]
                
movementRook (x,y) c g = up ++ down ++ right ++ left
    where
        up = block c g [(x,i)|i<-[borderMax (y+1) .. ]]
        down = block c g [(x,i)|i<-[borderMin (y-1), borderMin (y-2) .. ]]
        right = block c g [(i,y)|i<-[borderMax (x+1) .. ]]
        left = block c g [(i,y)|i<-[borderMin (x-1), borderMin (x-2) .. ]]
        
movementBishop (x,y) c g = 
    (generator (\(x,y)->(x+1,y+1))) ++ 
    (generator (\(x,y)->(x+1,y-1))) ++
    (generator (\(x,y)->(x-1,y+1))) ++
    (generator (\(x,y)->(x-1,y-1)))
    where
        generator f = block c g $ tail $ iterate f (x,y)
        
movementKnight a c g = []

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
