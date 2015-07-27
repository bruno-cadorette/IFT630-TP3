module Tp3.Chess (module ChessPieces, ChessGame(..),play, baseConfiguration) where

import qualified Data.Map as Map
import Ai
import ChessPieces
import Data.List


enemyColor White = Black
enemyColor Black = White

data ChessGame = ChessGame PieceColor Board

instance Show ChessGame where
    show (ChessGame color board) = showBoard
        where
            side = case color of
                White -> id
                Black -> reverse
            showBoard = unlines $ border ++ (map (showLine) $ side [7,6..0]) ++ border ++ (bottomLetters)
            showLine y =show (y+1) ++ " | " ++ (intercalate " | " $ map(\x-> caseToShow $ Map.lookup (x,y) board) (side [0..7])) ++ " |"
            caseToShow (Just x) = show x
            caseToShow Nothing = "  "
            border = ["  |" ++ (intercalate  "|" $ replicate 8 "----") ++ "|"]
            bottomLetters =["    " ++ (intercalate  "    " $ map(\i->[i]) $ side ['A'..'H'])]
        
    
instance Ai ChessGame where
    transition = transitionImpl
    heuristic (ChessGame color board) = 
        Map.fold(\p acc->if pieceColor p == color then (acc + (value p)) else (acc - (value p))) 0 board
    goal p = Nothing
    actions a = []

play :: ChessGame -> Position -> Position -> Maybe ChessGame
play (ChessGame color board) origin target = 
    case Map.lookup origin board of
        Just piece
            | elem target $ movement piece origin board -> 
                Just (ChessGame (enemyColor color) $ movePiece origin piece target board)
        _ -> Nothing
    
isPieceInDanger piecePos color board =
    any (\(k,v)-> elem piecePos $ movement v k board) $ filter (\(k,v)->pieceColor v /= color) $ Map.toList board
    
baseConfiguration :: ChessGame
baseConfiguration = (ChessGame White (Map.fromList $ whitePieces ++ blackPieces))
    where
    whitePieces = 
        ([((0,0), (ChessPiece Rook White)), ((7,0), (ChessPiece Rook White)), 
        ((1,0), (ChessPiece Knight White)), ((6,0), (ChessPiece Knight White)),
        ((2,0), (ChessPiece Bishop White)), ((5,0), (ChessPiece Bishop White)),
        ((3,0), (ChessPiece Queen White)), ((4,0), (ChessPiece King White))]) 
        ++ 
            map (\i -> ((i,1), (ChessPiece Pawn White))) [0..7]
    blackPieces = (map (\((x,y),(ChessPiece t _))->((x,7-y),(ChessPiece t Black)))) whitePieces
    

movePiece origin piece target board = Map.insert target piece $ Map.delete origin board
    
transitionImpl (ChessGame color board) = 
    concatMap transitions $ filter (\(k,x)->pieceColor x == color) $ Map.toList  board
    where
        transitions (pos, piece) = map (\t->ChessGame (enemyColor color) $ rebuild t) $ movement piece pos board
            where
                rebuild target = Map.insert target piece $ Map.delete pos board