{-# LANGUAGE DeriveDataTypeable #-}

module MPIServer where

import Control.Parallel.MPI.Simple (mpiWorld, commWorld, unitTag, send, recv)
import Control.Parallel.MPI.Base (anySource)
import Control.Parallel.MPI.Internal (Status)
import qualified Data.HashTable.IO as H
import HashCache
import Control.Monad
import Data.Binary
import Data.Typeable
import Data.Maybe
--import Tp3.Chess
import Data.Serialize
--0  c'est le jeu d'echec
--1 c'est la table
-- >1 c'est des noyaux de calcul

--data MPIRequestType = GetGameResult ChessGame | SetGameResult (ChessGame,Int) deriving (Serializable)
data MPIRequestType = GetGameResult [Char] | SetGameResult ([Char],Int) deriving (Typeable)

data MPIRequest = MPIRequest {
    sender :: Int,
    requestType :: MPIRequestType
} deriving (Typeable)
instance Binary MPIRequestType
instance Binary MPIRequest

calculEchec echec hashTable unitTag = ()

startTable size rank unitTag =
    --Start thread pr rcv chaque rank > 1 && < size
  do
    table <- getHashTable
    forever $ do
      (request,status) <- recv commWorld anySource unitTag
      sendResponse =<< handleRequest table (request,status)

main :: IO ()
main = mpiWorld $ \size rank ->
   if size < 3
      then putStrLn "At least two processes are needed"
      else case rank of
         0 -> do (msg, _status) <- recv commWorld 1 unitTag
                 putStrLn msg
         1 -> startTable size 1 unitTag
         --_ -> calculEchec 0 1 unitTag
sendResponse :: (Maybe Int,Int) -> IO()
sendResponse (Nothing,sender) = return ()
sendResponse (response,sender) =
        send commWorld  sender unitTag (fromJust response)

handleRequest :: H.BasicHashTable String Int -> (MPIRequest, Control.Parallel.MPI.Internal.Status) -> IO(Maybe Int,Int)
handleRequest table ((MPIRequest sender (GetGameResult chessGame)),status) =
  do
    value <- getValue table chessGame
    return (value, sender)
handleRequest table ((MPIRequest sender (SetGameResult (chessGame,value))),status) =
    do
      insertH table (chessGame) value
      return (Nothing,sender)
