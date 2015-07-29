module ArtificialIntelligence (Ai(..), minmax) where

import Data.List (maximumBy)
import Data.Function (on)


class (Show a)=>Ai a where
   transition :: a->[a]
   actions :: a->[(Action,a)]
   goal :: a->(Maybe Int)
   heuristic :: a->Int

type Action = String


--Depth Racine
minmax::(Ai a)=>Int -> a -> Action
minmax depth racine = fst $ maximumBy (compare `on` snd) (splitter (actions racine) depth)

interim::(Ai a)=>Int -> (Action,a) -> (Action,Int)
interim depth (action,node) = (action, minmax' depth False node)

--Depth isMax Node
minmax'::(Ai a)=>Int -> Bool -> a -> Int
minmax' 0 _ node = heuristic node
minmax' depth True node = maximum $ map (minmax' (depth-1) False) (transition node)
minmax' depth False node = minimum $ map (minmax' (depth-1) True) (transition node)

splitter::(Ai a)=>[(Action,a)] -> Int -> [(Action,Int)]
splitter listActions depth = map (interim depth) listActions --map (send commWorld 0 unitTag) listActions 
