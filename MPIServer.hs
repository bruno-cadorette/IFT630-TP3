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

type SenderRank = Int
data MPIRequestType =
    GetCache ChessGame SenderRank | 
    SetCache (ChessGame,Int) SenderRank | 
    GetGameResult ChessGame | 
    CancelComputation deriving (Generic)
    
    
instance Serialize MPIRequestType 

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
      