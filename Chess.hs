module Chess (module ChessPiece, module Chess, module ChessSerializer) where --  ChessGame(..),play, baseConfiguration) where

import qualified Data.Map as Map
--import ArtificialIntelligence
import AlgebraicChessNotation
import ChessPiece
import Data.List
import Data.Maybe
import ChessSerializer

enemyColor :: PieceColor -> PieceColor
enemyColor White = Black
enemyColor Black = White

instance PrettyPrint ChessGame where
    prettyPrint (ChessGame color (Board board)) = showBoard
        where
            side = case color of
                White -> id
                Black -> id
            showBoard = unlines $ border ++ (map (showLine) $ side [7,6..0]) ++ border ++ (bottomLetters)
            showLine y = show (y+1) ++ " | " ++ (intercalate " | " $ map(\x-> caseToShow $ Map.lookup (x,y) board) (side [0..7])) ++ " |"
            caseToShow (Just x) = prettyPrint x
            caseToShow Nothing = "  "
            border = ["  |" ++ (intercalate  "|" $ replicate 8 "----") ++ "|"]
            bottomLetters =["    " ++ (intercalate  "    " $ map(\i->[i]) $ side ['A'..'H'])]

{-
instance Ai ChessGame where
    transition = transitionImpl
    heuristic (ChessGame color (Board board)) =
        Map.fold(\p acc->if pieceColor p == color then (acc + (value p)) else (acc - (value p))) 0 board
    goal p = Nothing
    actions = actionsImpl-}

transitionImpl game = map snd $ actionsImpl game

play :: ChessGame -> Position -> Position -> Maybe ChessGame
play (ChessGame color board) origin target =
    case Map.lookup origin $ boardMap board of
        Just piece
            | elem target $ movement piece origin board ->
                Just (ChessGame (enemyColor color) $ movePiece origin piece target board)
        _ -> Nothing

isCheckMate :: ChessGame -> Bool
isCheckMate (ChessGame color board) =
    (isPieceInDanger (ChessGame color board) $ kingPos) &&
    (null $ transitionImpl (ChessGame color board))
    where
        kingPos = kingPosition (ChessGame color board)

kingPosition :: ChessGame -> Position
kingPosition (ChessGame color (Board board)) = fst $ fromJust $ find (\(_,c)->c == (ChessPiece King color)) $ Map.toList board

isKingInDanger :: ChessGame -> Bool
isKingInDanger game = isPieceInDanger game $ kingPosition game

isPieceInDanger :: ChessGame -> Position -> Bool
isPieceInDanger (ChessGame color board) piecePos =
    any (\(k,v)-> elem piecePos $ movement v k board) $ filter (\(_,v)->pieceColor v /= color) $ Map.toList $ boardMap board

baseConfiguration :: ChessGame
baseConfiguration = (ChessGame White (Board $ Map.fromList $ whitePieces ++ blackPieces))
    where
    whitePieces =
        ([((0,0), (ChessPiece Rook White)), ((7,0), (ChessPiece Rook White)),
        ((1,0), (ChessPiece Knight White)), ((6,0), (ChessPiece Knight White)),
        ((2,0), (ChessPiece Bishop White)), ((5,0), (ChessPiece Bishop White)),
        ((3,0), (ChessPiece Queen White)), ((4,0), (ChessPiece King White))])
        ++
            map (\i -> ((i,1), (ChessPiece Pawn White))) [0..7]
    blackPieces = (map (\((x,y),(ChessPiece t _))->((x,7-y),(ChessPiece t Black)))) whitePieces

movePiece :: Position -> ChessPiece -> Position -> Board -> Board
movePiece origin piece target (Board board) =Board $ Map.insert target piece $ Map.delete origin board

allActionsForBoard :: ChessGame -> (Position,ChessPiece) -> [(String, ChessGame)]
allActionsForBoard (ChessGame color board) (origin, piece) =
    filter (\(_, (ChessGame _ b)) -> not $ isKingInDanger (ChessGame color b)) $
    map (\target->((getNotation origin target), ChessGame (enemyColor color) $ movePiece origin piece target board)) $
    movement piece origin board

actionsImpl :: ChessGame -> [(String, ChessGame)]
actionsImpl (ChessGame color board) =
    concatMap (allActionsForBoard (ChessGame color board)) $ filter (\(_,x)->pieceColor x == color) $ Map.toList $ boardMap board
