module Tp3.Ai (Ai) where

class (Show a, Ord a)=>Ai a where
    transition :: a->[a]
    actions :: a->[(Action,a)]
    goal :: a->(Maybe Integer)
    heuristic :: a->Integer

type Action = String

--import Tp3.Ai
--import Data.Maybe
--import Data.List
--import Data.Ord

--test :: Test
--test = (0:[Nil, Nil])
--test = (0, 
--	[(2, 
--		[(3,
--			[(5, []),(7, [])]
--		),
--		(6,
--			[(2, []),(3, [])]
--		)]
--	),
--	(4,
--		[(3,
--			[(5, []),(7, [])]
--		),
--		(6,
--			[(2, []),(3, [])]
--		)]
--	)]
--       )

--Depth Racine
minmax::(Ai a)=>Integer -> a -> Action
minmax depth racine = fst $ bestAction (actions racine)
                      where --bestAction :: (Ai a)=>[(Action,a)] -> (Action,a)
                            bestAction (node:[]) = node
                            bestAction (node:nodes) | minmaxRecur (depth-1) False (snd node) > minmaxRecur (depth-1) False (snd (bestAction nodes)) = node 
                            bestAction (node:nodes) = bestAction nodes

--Depth isMax Node
minmaxRecur::(Ai a)=>Integer -> Bool -> a -> Integer
minmaxRecur 0 _ node = heuristic node
minmaxRecur depth True node = maximum $ map (minmaxRecur (depth-1) False) (transition node)
minmaxRecur depth False node = minimum $ map (minmaxRecur (depth-1) True) (transition node)
