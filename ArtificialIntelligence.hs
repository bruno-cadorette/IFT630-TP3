module ArtificialIntelligence (Ai(..), minmax) where

import Data.List (maximumBy)
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
minmax::(Ai a)=>DiffTime -> MVar -> a -> IO Action
minmax duration isDone racine = fmap (fst . maximumBy (compare `on` snd)) (splitter (actions racine) isDone duration)

interim::(Ai a)=>DiffTime -> MVar -> (Action,a) -> IO (Action,Int)
interim duration isDone (action,node) = do 
				startTime <- fmap utctDayTime getCurrentTime
				minmax <- minmax' startTime duration isDone False node
				return (action, minmax)

--Depth isMax Node
minmax'::(Ai a)=>DiffTime -> MVar -> DiffTime -> Bool -> a -> IO Int
minmax' startTime duration isDone True node = fmap (maximum) (mapM (minmaxOver startTime duration isDone False) (transition node))
minmax' startTime duration isDone False node = fmap (minimum) (mapM (minmaxOver startTime duration isDone True) (transition node))

minmaxOver::(Ai a)=>DiffTime -> MVar -> DiffTime -> Bool -> a -> IO Int
minmaxOver startTime duration isDone bool node = do
					currentTime <- fmap utctDayTime getCurrentTime
					block <- tryReadMVar isDone
					if startTime + duration > currentTime || isJust block
						then return $ heuristic node
						else minmax' startTime duration isDone bool node

splitter::(Ai a)=>[(Action,a)] -> DiffTime -> MVar -> IO [(Action,Int)]
splitter listActions duration isDone = mapM (interim duration isDone) listActions --map (send commWorld 0 unitTag) listActions 
