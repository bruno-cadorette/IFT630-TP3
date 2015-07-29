{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}



import Control.Parallel.MPI.Simple (mpiWorld, commWorld, unitTag, send, recv)
import Control.Parallel.MPI.Base (anySource)
import Control.Parallel.MPI.Internal (Status,toRank,fromRank)
import Control.Concurrent (threadDelay)
--import Control.Distributed.Process.Serializable
import GHC.Generics
import qualified Data.HashTable.IO as H
import HashCache
import Control.Monad
import Data.Maybe
import Data.Serialize
import Chess

--0  c'est le jeu d'echec
--1 c'est la table
-- >1 c'est des noyaux de calcul

--data MPIRequestType = GetGameResult ChessGame | SetGameResult (ChessGame,Int) deriving (Serializable)
data MPIRequestType = GetGameResult [Char] | SetGameResult ([Char],Int) deriving (Generic)

data MPIRequest = MPIRequest {
    sender :: Int,
    requestType :: MPIRequestType
} deriving (Generic)

instance Serialize MPIRequestType {-where
      put (GetGameResult a) = putWord8 0 >> put a
      put (SetGameResult b) = putWord8 1 >> put b
      get = do
        tag <- getWord8
        case tag of
         0 -> do
                a <- get
                return (GetGameResult a)
         _ -> do
                a <- get
                return (SetGameResult a)-}

instance Serialize MPIRequest {-where
      put (MPIRequest sndr rqst) = do
        put sndr
        put rqst
      get = do
        (MPIRequest sndr rqst) <- get
        return (MPIRequest sndr rqst) -}


--Ce que j,attend! (Int sender,(Bool isGet,String Hash du jeu,Int cost))
--Dans le cas d'un get je me fou du cost donc mettez ce que vous voulez

receiver = 0
sender2 = 1

calculEchec echec hashTable unitTag = ()

startTable size rank unitTag =
    --Start thread pr rcv chaque rank > 1 && < size
  do
    table <- getHashTable

    (print "Waiting")
    (request,status) <- recv commWorld anySource unitTag
    (putStrLn "We got a msg")
    sendResponse =<< handleRequest table (request,status)


main :: IO ()
main = mpiWorld $ \size rank ->
   if size < 2
      then putStrLn "At least three processes are needed"
      else case rank of
         1 -> do
                     threadDelay 500
                     print "Sending"
                     send commWorld receiver unitTag (MPIRequest (fromRank sender2) (GetGameResult "abc"))
                     print "Sent"
         0 -> startTable size receiver unitTag
         _ -> return()
         --_ -> calculEchec 0 1 unitTag
sendResponse :: (Maybe Int,Int) -> IO()
sendResponse (Nothing,_) = return ()
sendResponse (response,sender) =
        send commWorld  (toRank sender) unitTag (fromJust response)

handleRequest :: H.BasicHashTable String Int -> (MPIRequest, Control.Parallel.MPI.Internal.Status) -> IO(Maybe Int,Int)
handleRequest table ((MPIRequest sender (GetGameResult chessGame)),_) =
  do
    cost <- getValue table chessGame
    return (cost, sender)
handleRequest table ((MPIRequest sender (SetGameResult (chessGame,value))),_) =
    do
      insertH table (chessGame) value
      return (Nothing,sender)
      
testSerialize size rank = 
    if size < 2= then 
        putStrLn "At least two processes are needed"
    else 
        case rank of
            0 -> do (msg, _status) <- recv commWorld 1 unitTag
                 putStrLn $ prettyPrint msg
            1 -> send commWorld 0 unitTag baseConfiguration
            _ -> return ()
