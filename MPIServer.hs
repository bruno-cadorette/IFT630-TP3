{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MPIServer where

import Control.Parallel.MPI.Simple (mpiWorld, commWorld, unitTag, send, recv)
import Control.Parallel.MPI.Base (anySource)
import Control.Parallel.MPI.Internal (Status,toRank)
--import Control.Distributed.Process.Serializable
import qualified Data.HashTable.IO as H
import HashCache
import Control.Monad
import Data.Binary
import Data.Typeable
import Data.Maybe
import GHC.Generics
--import Tp3.Chess
--0  c'est le jeu d'echec
--1 c'est la table
-- >1 c'est des noyaux de calcul

--data MPIRequestType = GetGameResult ChessGame | SetGameResult (ChessGame,Int) deriving (Serializable)
{-data MPIRequestType = GetGameResult [Char] | SetGameResult ([Char],Int) deriving (Typeable, Generic)

data MPIRequest = MPIRequest {
    sender :: Int,
    requestType :: (Bool, String, Int)
} deriving (Typeable,Generic)
instance Binary MPIRequestType
instance Binary MPIRequest-}

--Ce que j,attend! (Int sender,(Bool isGet,String Hash du jeu,Int cost))
--Dans le cas d'un get je me fou du cost donc mettez ce que vous voulez

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
      then putStrLn "At least three processes are needed"
      else case rank of
         0 -> do (msg, _status) <- recv commWorld 1 unitTag
                 putStrLn msg
         1 -> startTable size 1 unitTag
         _ -> return()
         --_ -> calculEchec 0 1 unitTag
sendResponse :: (Maybe Int,Int) -> IO()
sendResponse (Nothing,_) = return ()
sendResponse (response,sender) =
        send commWorld  (toRank sender) unitTag (fromJust response)

handleRequest :: H.BasicHashTable String Int -> ((Int,(Bool,String,Int)), Control.Parallel.MPI.Internal.Status) -> IO(Maybe Int,Int)
handleRequest table (( sender, (True, chessGame,_)),_) =
  do
    cost <- getValue table chessGame
    return (cost, sender)
handleRequest table (( sender, (False, chessGame,value)),_) =
    do
      insertH table (chessGame) value
      return (Nothing,sender)
