{-# LANGUAGE DeriveGeneric #-}
module Interact (human,Player,ai,playGame,playTurn,printGame,computeNode') where


import Chess
import AlgebraicChessNotation
import ArtificialIntelligence
import Data.Maybe
import qualified Data.Map as Map
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Data.Serialize
import GHC.Generics
import Data.Time.Clock
import GHC.Conc (numCapabilities)
type Player = ChessGame -> IO(Maybe ChessGame)

type SenderRank = Int
data MPIRequestType =
    GetCache ChessGame SenderRank |
    SetCache (ChessGame,Int) SenderRank |
    GetGameResult ChessGame |
    CancelComputation deriving (Generic)


instance Serialize MPIRequestType

verifyCheckMate game =
    if isCheckMate game then
        Nothing
    else
        Just game
human :: Player
human game = do
    (origin, target) <- fmap getMoveInput getLine
    case play game origin target of
        Just game' -> return $ verifyCheckMate game'
        Nothing -> do
            putStrLn "Le mouvement est invalide!"
            human game

ai :: DiffTime -> Player
ai depth game = do return $ verifyCheckMate $ fromJust $ play game origin target
    where
        (origin, target) = getMoveInput $ minmax depth newEmptyMVar game

playGame :: Player -> Player -> IO()
playGame whitePlayer blackPlayer =
    playTurn baseConfiguration $ cycle [whitePlayer, blackPlayer]

playTurn :: ChessGame -> [Player] -> IO ()
playTurn game (f:fs) = do
    printGame game
    putStrLn ""
    g <- f game
    case g of
        Just game' -> playTurn game' fs
        Nothing -> print "Échec et mat"

printGame :: ChessGame -> IO()
printGame = putStrLn . prettyPrint



start = ChessGame White (Board {boardMap = Map.fromList [((0,0),ChessPiece {pieceType = Rook, pieceColor = White}),((0,1),ChessPiece {pieceType = Pawn, pieceColor = White}),((0,2),ChessPiece {pieceType = Rook, pieceColor = White}),((0,6),ChessPiece {pieceType = Pawn, pieceColor = Black}),((0,7),ChessPiece {pieceType = Rook, pieceColor = Black}),((1,0),ChessPiece {pieceType = Knight, pieceColor = White}),((1,1),ChessPiece {pieceType = Pawn, pieceColor = White}),((1,5),ChessPiece {pieceType = Rook, pieceColor = Black}),((1,6),ChessPiece {pieceType = Pawn, pieceColor = Black}),((1,7),ChessPiece {pieceType = Knight, pieceColor = Black}),((2,0),ChessPiece {pieceType = Bishop, pieceColor = White}),((2,1),ChessPiece {pieceType = Pawn, pieceColor = White}),((2,6),ChessPiece {pieceType = Pawn, pieceColor = Black}),((2,7),ChessPiece {pieceType = Bishop, pieceColor = Black}),((3,0),ChessPiece {pieceType = Queen, pieceColor = White}),((3,1),ChessPiece {pieceType = Pawn, pieceColor = White}),((3,6),ChessPiece {pieceType = Pawn, pieceColor = Black}),((3,7),ChessPiece {pieceType = Queen, pieceColor = Black}),((4,0),ChessPiece {pieceType = King, pieceColor = White}),((4,1),ChessPiece {pieceType = Pawn, pieceColor = White}),((4,6),ChessPiece {pieceType = Pawn, pieceColor = Black}),((4,7),ChessPiece {pieceType = King, pieceColor = Black}),((5,0),ChessPiece {pieceType = Bishop, pieceColor = White}),((5,1),ChessPiece {pieceType = Pawn, pieceColor = White}),((5,6),ChessPiece {pieceType = Pawn, pieceColor = Black}),((5,7),ChessPiece {pieceType = Bishop, pieceColor = Black}),((6,0),ChessPiece {pieceType = Knight, pieceColor = White}),((6,1),ChessPiece {pieceType = Pawn, pieceColor = White}),((6,6),ChessPiece {pieceType = Pawn, pieceColor = Black}),((6,7),ChessPiece {pieceType = Knight, pieceColor = Black}),((7,3),ChessPiece {pieceType = Pawn, pieceColor = White}),((7,4),ChessPiece {pieceType = Pawn, pieceColor = Black})]})




computeNode' duration send rcv flags = do
    (msg, status) <- rcv
    case msg of
        GetGameResult game -> do
            printGame game
            stopFlag <- newEmptyMVar
            action <- minmax duration stopFlag game
            forkIO $ send action  -- Mettre le flag dans minmax
            computeNode' duration send rcv  (stopFlag:flags)
        CancelComputation -> do
            putStrLn "Task cancelled"
            mapM_ (\x-> putMVar x ()) flags
        _ -> error "Bad type MPI Request"


{-main :: IO ()
main = do
    --mapM_ print [getNotation x y|x<-[(i,j)|i<-[0..7],j<-[0..7]], y<-[(i,j)|i<-[0..7],j<-[0..7]]]
    --test baseConfiguration
    --print $ getMoveInput "b1-'0"
   -- printState start
    --mapM_ (mapM_ printState . transition) $ transition $ start
    computeNode
    --playGame (ai 4) (ai 4)
    putStrLn "Fin de la partie"-}
