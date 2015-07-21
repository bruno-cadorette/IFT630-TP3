module Main where

import Control.Parallel.MPI.Simple (mpiWorld, commWorld, unitTag, send, recv)
--0  c'est le jeu d'echec
--1 c'est la table
-- >1 c'est des noyaux de calcul


calculEchec echec hashTable unitTag =
 do (hash,_status) <- recv commWorld echec unitTag
    1

startTable size rank unitTag =
    --Start thread pr rcv chaque rank > 1 && < size

main :: IO ()
main = mpiWorld $ \size rank ->
   if size < 3
      then putStrLn "At least two processes are needed"
      else case rank of
         0 -> do (msg, _status) <- recv commWorld 1 unitTag
                 putStrLn msg
         1 -> startTable size 1 unitTag
         _ -> calculEchec 0 1 unitTag
