import Tp3.Chess
import AlgebricChessNotation

nextTurn :: ChessGame -> IO ChessGame
nextTurn game = do
    print game
    (origin, target) <- fmap getMoveInput getLine
    case play game origin target of
        Just game' -> nextTurn game'
        Nothing -> do
            putStrLn "Le mouvement est invalide!"
            nextTurn game
main = nextTurn baseConfiguration