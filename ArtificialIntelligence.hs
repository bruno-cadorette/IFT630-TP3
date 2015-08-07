module ArtificialIntelligence (Ai(..), Action, interim, minmax) where

import Control.Parallel.MPI.Simple (mpiWorld, commWorld, unitTag, send, recv)
import Control.Parallel.MPI.Base (anySource)
import Control.Parallel.MPI.Internal (Status,toRank,fromRank)
import Data.List
import Data.List.Split
import Data.Function (on)
import Data.Time.Clock
import Control.Monad
import Control.Concurrent.MVar
import Data.Maybe
import MPIRequestType
import Data.Serialize
import qualified Data.Map as Map
import Chess


class (Serialize a , Show a)=>Ai a where
   transition :: a->[a]
   actions :: a->[(Action,a)]
   heuristic :: a->Int

instance Ai ChessGame where
   transition = transitionImpl
   heuristic (ChessGame color (Board board)) =
       Map.fold(\p acc->if pieceColor p == color then (acc + (value p)) else (acc - (value p))) 0 board
   actions = actionsImpl



--Depth Racine
minmax::Integer -> MVar() -> Int -> ChessGame -> IO Action
minmax duration isDone size racine = fmap (fst . maximumBy (compare `on` snd)) (splitter (actions racine)   duration size isDone)

interim::Integer -> MVar() -> (Action,ChessGame) -> IO (Action,Int)
interim duration isDone (action,node) = do
				startTime <- fmap utctDayTime getCurrentTime
				res <- minmax' startTime (secondsToDiffTime duration) isDone False node
				print "On a le res dans itnerim"
				return (action, res)

--Depth isMax Node
minmax'::DiffTime ->  DiffTime -> MVar() -> Bool -> ChessGame -> IO Int
minmax' startTime duration isDone True node = fmap (maximum) (mapM (minmaxOver startTime duration isDone False) (transition node))
minmax' startTime duration isDone False node = fmap (minimum) (mapM (minmaxOver startTime duration isDone True) (transition node))

minmaxOver::DiffTime -> DiffTime -> MVar() ->Bool -> ChessGame -> IO Int
minmaxOver startTime duration isDone bool node = do
					currentTime <- fmap utctDayTime getCurrentTime
					block <- tryReadMVar isDone
					if startTime + duration < currentTime || isJust block
						then do
					return $ heuristic node
						else do
					minmax' startTime duration isDone bool node

splitter::[(Action,ChessGame)] -> Integer -> Int -> MVar() -> IO [(Action,Int)]
splitter listActions duration size isDone = do
					let (xs:xss) = splitIn (size) listActions
					let ourActions = mapM (interim duration isDone) xs
					mapM_ (\(f,s) -> sending s f duration) (zip xss [1..])
					liftM2 (++) (buildLists [1.. (size-1)]) ourActions

--buildLists :: [Int]->IO [(Action,Int)]
buildLists [] = return []
buildLists remainings = do
			print "on recv"
			(msg,_) <- recv commWorld anySource unitTag
			case msg of
				ReturnGameResult actions sender ->do
					print "Receive"
					fmap (\xs -> actions : xs)  $ buildLists (delete sender remainings)
				_ -> error "Mauvais Type"

sending ::  Int -> [(Action, ChessGame)]->Integer -> IO ()
sending numero listActions duration =
  do
   print $ "On send" ++  (show numero)
   mapM_ (\x -> send commWorld (toRank numero) unitTag (GetGameResult duration x)) listActions

splitIn :: Integral a => a -> [e] -> [[e]]
splitIn n xs = splitPlaces (splitIn' (fromIntegral n) (genericLength xs)) xs
               where
               splitIn' n total
                   | n == total || total == 0 || n == 0 = []
               splitIn' n total =
                   let total' =  ceiling $ total / n
                   in total':splitIn' (n-1) (fromIntegral $ (floor total) - total')
