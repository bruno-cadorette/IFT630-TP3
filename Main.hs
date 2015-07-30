import Chess
import AlgebraicChessNotation
import Data.Serialize
import ArtificialIntelligence
import Data.Maybe
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
    g <- f game
    case g of
        Just game' -> playTurn game' fs
        Nothing -> print "Échec et mat"

printState :: ChessGame -> IO()
printState = putStrLn . prettyPrint
        
main :: IO ()
main = do
    playGame (ai 1) (ai 1)
    putStrLn "Fin de la partie"
    