import Chess
import AlgebraicChessNotation
import ArtificialIntelligence
import Data.Maybe
import qualified Data.Map as Map 
import Control.Applicative

type Player = ChessGame -> IO(Maybe ChessGame)

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

ai :: Int -> Player
ai depth game = do return $ verifyCheckMate $ fromJust $ play game origin target
    where 
        (origin, target) = getMoveInput $ minmax depth game 
        
playGame :: Player -> Player -> IO()
playGame whitePlayer blackPlayer = 
    playTurn baseConfiguration $ cycle [whitePlayer, blackPlayer]
    
playTurn :: ChessGame -> [Player] -> IO ()
playTurn game (f:fs) = do
    printState game
    putStrLn ""
    g <- f game
    case g of
        Just game' -> playTurn game' fs
        Nothing -> print "Échec et mat"

printState :: ChessGame -> IO()
printState = putStrLn . prettyPrint
        
start = ChessGame White (Board {boardMap = Map.fromList [((0,0),ChessPiece {pieceType = Rook, pieceColor = White}),((0,1),ChessPiece {pieceType = Pawn, pieceColor = White}),((0,2),ChessPiece {pieceType = Rook, pieceColor = White}),((0,6),ChessPiece {pieceType = Pawn, pieceColor = Black}),((0,7),ChessPiece {pieceType = Rook, pieceColor = Black}),((1,0),ChessPiece {pieceType = Knight, pieceColor = White}),((1,1),ChessPiece {pieceType = Pawn, pieceColor = White}),((1,5),ChessPiece {pieceType = Rook, pieceColor = Black}),((1,6),ChessPiece {pieceType = Pawn, pieceColor = Black}),((1,7),ChessPiece {pieceType = Knight, pieceColor = Black}),((2,0),ChessPiece {pieceType = Bishop, pieceColor = White}),((2,1),ChessPiece {pieceType = Pawn, pieceColor = White}),((2,6),ChessPiece {pieceType = Pawn, pieceColor = Black}),((2,7),ChessPiece {pieceType = Bishop, pieceColor = Black}),((3,0),ChessPiece {pieceType = Queen, pieceColor = White}),((3,1),ChessPiece {pieceType = Pawn, pieceColor = White}),((3,6),ChessPiece {pieceType = Pawn, pieceColor = Black}),((3,7),ChessPiece {pieceType = Queen, pieceColor = Black}),((4,0),ChessPiece {pieceType = King, pieceColor = White}),((4,1),ChessPiece {pieceType = Pawn, pieceColor = White}),((4,6),ChessPiece {pieceType = Pawn, pieceColor = Black}),((4,7),ChessPiece {pieceType = King, pieceColor = Black}),((5,0),ChessPiece {pieceType = Bishop, pieceColor = White}),((5,1),ChessPiece {pieceType = Pawn, pieceColor = White}),((5,6),ChessPiece {pieceType = Pawn, pieceColor = Black}),((5,7),ChessPiece {pieceType = Bishop, pieceColor = Black}),((6,0),ChessPiece {pieceType = Knight, pieceColor = White}),((6,1),ChessPiece {pieceType = Pawn, pieceColor = White}),((6,6),ChessPiece {pieceType = Pawn, pieceColor = Black}),((6,7),ChessPiece {pieceType = Knight, pieceColor = Black}),((7,3),ChessPiece {pieceType = Pawn, pieceColor = White}),((7,4),ChessPiece {pieceType = Pawn, pieceColor = Black})]})
                
main :: IO ()
main = do
    --mapM_ print [getNotation x y|x<-[(i,j)|i<-[0..7],j<-[0..7]], y<-[(i,j)|i<-[0..7],j<-[0..7]]]
    --test baseConfiguration
    --print $ getMoveInput "b1-'0"
   -- printState start
    --mapM_ (mapM_ printState . transition) $ transition $ start
    playGame (ai 4) (ai 4)
    putStrLn "Fin de la partie"
    