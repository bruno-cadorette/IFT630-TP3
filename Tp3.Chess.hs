module Tp3.Chess (ChessGame(..), PieceColor(..), PieceType(..)) where

import qualified Data.Map as Map
import Tp3.Ai

type Board = Map.Map (Int,Int) ChessPiece

class (Show p)=>Piece p where
    value::p->Integer
    movement::p->(Int,Int)->Board->[(Int,Int)]
    
data PieceColor = White | Black deriving(Eq,Show)

enemyColor White = Black
enemyColor Black = White

data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving(Show)

data ChessPiece = ChessPiece {
    pieceType :: PieceType ,
    pieceColor :: PieceColor
} deriving(Show)

data ChessGame = ChessGame PieceColor Board

instance Piece ChessPiece where
    value (ChessPiece t _)= pieceValue t
    movement = movementImpl

instance Show ChessGame where
    show (ChessGame color board) = show board
    
instance Ai ChessGame where
    transition = transitionImpl
    heuristic (ChessGame color board) = 
        Map.fold(\p acc->if pieceColor p == color then (acc + (value p)) else (acc - (value p))) 0 board
    goal p = Nothing
    actions a = []
    
    
transitionImpl (ChessGame color board) = 
    concatMap transitions $ filter (\(k,x)->pieceColor x == color) $ Map.toList  board
    where
        transitions (pos, elem) = map (\t->ChessGame (enemyColor color) $ rebuild t) $ movement elem pos board
            where
                rebuild target = Map.insert target elem $ Map.delete pos board
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

    
borderMin = max 0
borderMax = min 7

isOutOfMap (x,y) = x >= 8 || y >= 8 || x < 0 || y < 0
canEat pos myColor board = 
    case Map.lookup pos board of 
        Just(x) -> pieceColor x /= myColor
        Nothing -> True
        
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

movementKing (x,y) c g = filter (\(i,j)-> (not (isOutOfMap (i,j))) && (not (x == i && y == j)) && canEat (i,j) c g) possibleMovements
    where
        possibleMovements = 
            [(i,j)|i<-[(borderMin x-1)..(borderMax x+1)], j<-[(borderMin y-1)..(borderMax y+1)]]
        
        
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
        
movementKnight (x,y) c g = filter(\pos->not (isOutOfMap pos) || canEat pos c g ) $ map (\(i,j)->(x+i,y+j)) possible
    where
        possible = [(-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1)]
movementPawn (x,y) c g = filter (\pos->not $ isOutOfMap pos) $ [(x,y+d)] ++ pawnEat ++ firstMove
    where
    pawnEat = filter pawnEat' [(x-1,y + d),(x+1,y+d)]
        where 
        pawnEat' pos =
            case Map.lookup pos g of 
                Just(x)->pieceColor x /= c
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