import Chess
import AlgebraicChessNotation

nextTurn :: ChessGame -> IO ()
nextTurn game = do
    putStrLn $ prettyPrint game
    (origin, target) <- fmap getMoveInput getLine
    case play game origin target of
        Just game' -> 
            if isCheckMate game' then
                print "Échec et mat!!!"
            else do
                nextTurn game'
        Nothing -> do
            putStrLn "Le mouvement est invalide!"
            nextTurn game
            
            
            
main :: IO ()
main = nextTurn baseConfiguration