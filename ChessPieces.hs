module ChessPieces(
    Position, 
    Board, 
    Piece(..), 
    PieceColor(..), 
    PieceType(..), 
    ChessPiece(..)) where
    
import qualified Data.Map as Map    
    
type Position = (Int,Int)
type Board = Map.Map Position ChessPiece

class (Show p)=>Piece p where
    value::p->Int
    movement::p->Position->Board->[Position]
    
data PieceColor = White | Black deriving (Enum, Eq)

data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving (Enum)

data ChessPiece = ChessPiece {
    pieceType :: PieceType,
    pieceColor :: PieceColor
    
}

instance Show PieceType where
    show King = "K"
    show Queen = "Q"
    show Rook = "R"
    show Bishop = "B"
    show Knight = "N"
    show Pawn = "P"

instance Show PieceColor where 
    show White = "W"
    show Black = "B"

instance Show ChessPiece where
    show (ChessPiece t c) = show c ++ show t
    
instance Piece ChessPiece where
    value (ChessPiece t _)= pieceValue t
    movement = movementImpl

borderMin :: Int -> Int    
borderMin = max 0
borderMax :: Int -> Int   
borderMax = min 7


isOutOfMap :: Position -> Bool
isOutOfMap (x,y) = x >= 8 || y >= 8 || x < 0 || y < 0
canEat :: Position -> PieceColor -> Board -> Bool 
canEat pos myColor board = 
    case Map.lookup pos board of 
        Just(x) -> pieceColor x /= myColor
        Nothing -> True
        
block :: PieceColor -> Board -> [Position] -> [Position]
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
                    
pieceValue :: PieceType -> Int                    
pieceValue King = 1000
pieceValue Queen = 9
pieceValue Rook = 5
pieceValue Bishop = 3
pieceValue Knight = 3
pieceValue Pawn = 1

movementImpl :: ChessPiece -> Position -> Board -> [Position]
movementImpl (ChessPiece t color) xy board = movementKind xy color board
    where 
        movementKind = 
            case t of
                King -> movementKing
                Queen -> movementQueen
                Rook -> movementRook
                Bishop -> movementBishop
                Knight -> movementKnight
                Pawn -> movementPawn
movementKing :: Position -> PieceColor -> Board -> [Position]
movementKing (x,y) c g = filter (\(i,j)-> (not (isOutOfMap (i,j))) && (not (x == i && y == j)) && canEat (i,j) c g) possibleMovements
    where
        possibleMovements = 
            [(i,j)|i<-[(borderMin x-1)..(borderMax x+1)], j<-[(borderMin y-1)..(borderMax y+1)]]
        
movementQueen :: Position -> PieceColor -> Board -> [Position]
movementQueen pos c g = concatMap (\f-> f pos c g) [movementRook, movementBishop]

movementRook :: Position -> PieceColor -> Board -> [Position]
movementRook (x,y) c g = up ++ down ++ right ++ left
    where
        up = block c g [(x,i)|i<-[borderMax (y+1) .. ]]
        down = block c g [(x,i)|i<-[borderMin (y-1), borderMin (y-2) .. ]]
        right = block c g [(i,y)|i<-[borderMax (x+1) .. ]]
        left = block c g [(i,y)|i<-[borderMin (x-1), borderMin (x-2) .. ]]
            
movementBishop :: Position -> PieceColor -> Board -> [Position]
movementBishop (x,y) c g = 
    (generator (\(x',y')->(x'+1,y'+1))) ++ 
    (generator (\(x',y')->(x'+1,y'-1))) ++
    (generator (\(x',y')->(x'-1,y'+1))) ++
    (generator (\(x',y')->(x'-1,y'-1)))
    where
        generator f = block c g $ tail $ iterate f (x,y)
        
movementKnight :: Position -> PieceColor -> Board -> [Position]
movementKnight (x,y) c g = filter(\pos->not (isOutOfMap pos) || canEat pos c g ) $ map (\(i,j)->(x+i,y+j)) possible
    where
        possible = [(-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1)]
        
movementPawn :: Position -> PieceColor -> Board -> [Position]
movementPawn (x,y) c g = filter (\pos->not $ isOutOfMap pos) $ [(x,y+d)] ++ pawnEat ++ firstMove
    where
    pawnEat = filter pawnEat' [(x-1,y + d),(x+1,y+d)]
        where 
        pawnEat' pos =
            case Map.lookup pos g of 
                Just(other)->pieceColor other /= c
                Nothing->False
    firstMove = if fm then [(x,y+2*d)] else []
        where 
        fm =
            case c of
                White->y==1
                Black->y==6
    d = 
        case c of
            White-> 1
            Black-> -1