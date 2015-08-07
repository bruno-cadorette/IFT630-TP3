module ArtificialIntelligence (Ai(..), Action, interim, minmax) where

import Control.Parallel.MPI.Simple (mpiWorld, commWorld, unitTag, send, recv)
import Control.Parallel.MPI.Base (anySource)
import Control.Parallel.MPI.Internal (Status,toRank,fromRank)
import MPIRequestType
import Data.List (maximumBy)
import Data.List.Split
import Data.Function (on)
import Data.Time.Clock
import Control.Concurrent.MVar
import Data.Maybe


class (Show a)=>Ai a where
   transition :: a->[a]
   actions :: a->[(Action,a)]
   goal :: a->(Maybe Int)
   heuristic :: a->Int

type Action = String


--Depth Racine
minmax::(Ai a)=>Integer -> MVar() -> Int -> a -> IO Action
minmax duration isDone size racine = fmap (fst . maximumBy (compare `on` snd)) (splitter (actions racine)  duration isDone)

interim::(Ai a)=>Integer -> MVar() -> (Action,a) -> IO (Action,Int)
interim duration isDone (action,node) = do
				startTime <- fmap utctDayTime getCurrentTime
				minmax <- minmax' startTime (secondsToDiffTime duration) isDone False node
				return (action, minmax)

--Depth isMax Node
minmax'::(Ai a)=>DiffTime ->  DiffTime -> MVar() -> Bool -> a -> IO Int
minmax' startTime duration isDone True node = fmap (maximum) (mapM (minmaxOver startTime duration isDone False) (transition node))
minmax' startTime duration isDone False node = fmap (minimum) (mapM (minmaxOver startTime duration isDone True) (transition node))

minmaxOver::(Ai a)=>DiffTime -> DiffTime -> MVar() ->Bool -> a -> IO Int
minmaxOver startTime duration isDone bool node = do
					currentTime <- fmap utctDayTime getCurrentTime
					block <- tryReadMVar isDone
					if startTime + duration > currentTime || isJust block
						then return $ heuristic node
						else minmax' startTime duration isDone bool node

splitter::(Ai a)=>[(Action,a)] -> Integer -> Integer -> MVar() -> IO [(Action,Int)]
splitter listActions duration size isDone = do
					(xs:xss) <- (splitIn (size-1) listActions)
					let ourActions = interim duration isDone xs
					mapM ((f,s) -> sending s f duration) (zip xss [2..])
					ourActions `liftM2 (++)` (buildLists [2..size-1])
					

buildLists [] = []
buildLists remainings = do
			(msg,_) <- recv commWorld anySource unitTag
			case msg of
				ReturnGameResult actions sender -> actions : buildLists (delete sender remainings)
				_ -> error "Mauvais Type"					

sending numero listActions duration = map ((f,s) -> send commWorld (toRank numero) unitTag (GetGameResult duration (f,s))) listActions

splitIn n xs = splitPlaces (splitIn' n (length xs)) xs
    where
	splitIn' n 0 = []
        splitIn' n total = 
            let total' = ceiling $ total / n
            in total':splitIn' n total'

