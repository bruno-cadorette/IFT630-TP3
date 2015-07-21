module Tp3.Ai (Ai) where

class (Show a, Ord a)=>Ai a where
    transition :: a->[(Action,a)]
    goal :: a->(Maybe Integer)
    heuristic :: a->Integer

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

--Depth isMax Node
minmax::(Ai a)=>Integer -> Bool -> a -> Integer
minmax 0 _ node = heuristic node
minmax _ _ node = heuristic node
minmax depth True node = maximum $ map (minmax (depth-1) False) (transition node)
minmax depth False node = minimum $ map (minmax (depth-1) True) (transition node)


--minmax::(Ai a)=>a->action
--minmax = tourMax
--    where
--        tourMin ai = fromMaybe (minimumBy (comparing snd)$ map tourMax $ transition ai) (but ai)
--        tourMax ai = fromMaybe (maximumBy (comparing snd)$ map tourMin $ transition ai) (but ai)
