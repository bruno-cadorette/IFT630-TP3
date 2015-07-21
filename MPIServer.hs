module Main where

import Control.Parallel.MPI.Simple (mpiWorld, commWorld, unitTag, send, recv)
--0  c'est le jeu d'echec
--1 c'est la table
-- >1 c'est des noyaux de calcul


CalculEchec echec hashTable unitTag =
 do (hash,_status) <- recv commWorld echec unitTag
    echec = UnHash(hash)

main :: IO ()
main = mpiWorld $ \size rank ->
   if size < 3
      then putStrLn "At least two processes are needed"
      else case rank of
         0 -> do (msg, _status) <- recv commWorld 1 unitTag
                 putStrLn msg
         1 -> send commWorld 0 unitTag "Hello World"
         _ -> CalculEchec 0 1 unitTag
