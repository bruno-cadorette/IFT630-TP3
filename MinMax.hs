import Data.List (maximumBy)
import Data.Function (on)
import Ai

type Action = String

--Depth Racine
minmax::(Ai a)=>Integer -> a -> Action
minmax depth racine = fst $ maximumBy (compare `on` (interim depth)) (actions racine)

interim::(Ai a)=>Integer -> (Action,a) -> Integer
interim depth (action,node) = minmax' depth False node

--Depth isMax Node
minmax'::(Ai a)=>Integer -> Bool -> a -> Integer
minmax' 0 _ node = heuristic node
minmax' depth True node = maximum $ map (minmax' (depth-1) False) (transition node)
minmax' depth False node = minimum $ map (minmax' (depth-1) True) (transition node)
