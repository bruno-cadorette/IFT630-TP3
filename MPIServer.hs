{-# LANGUAGE DeriveGeneric #-}



import Control.Parallel.MPI.Simple (mpiWorld, commWorld, unitTag, send, recv)
import Control.Parallel.MPI.Base (anySource)
import Control.Parallel.MPI.Internal (Status,toRank,fromRank)
import Control.Concurrent (threadDelay)
--import Control.Distributed.Process.Serializable
import GHC.Generics
import qualified Data.HashTable.IO as H
import Control.Monad
import Data.Maybe
import Data.Serialize
import Chess
import Data.ByteString
import Data.Time.Clock
import Interact

--0  c'est le jeu d'echec
--1 c'est la table
-- >1 c'est des noyaux de calcul
type HashTable k v = H.BasicHashTable k v



calculEchec echec hashTable unitTag = ()



main :: IO ()
main = mpiWorld $ \size rank ->
   if size < 2
      then print "At least three processes are needed"
      else case rank of
         0 -> do
                playGame (ai size 3) (ai size 3)
         _ -> computeNode (fromRank rank) send' receive'
                where
                    receive' = recv commWorld (toRank 0) unitTag
                    send' = send commWorld  (toRank 0) unitTag

sendResponse :: (Maybe Int,Int) -> IO()
sendResponse (Nothing,_) = return ()
sendResponse (response,sender) =
        do
        print "On send"
        send commWorld  (toRank sender) unitTag (fromJust response)

{-handleRequest :: H.BasicHashTable ByteString Int -> (MPIRequestType, Control.Parallel.MPI.Internal.Status) -> IO(Maybe Int,Int)
handleRequest table ((GetCache chessGame sender), _) =
  do
    cost <- H.lookup table (encode chessGame)
    return (cost, sender)
handleRequest table ((SetCache (chessGame,value) sender), _) =
    do
      H.insert table (encode chessGame) value
      return (Nothing,sender)
-}
