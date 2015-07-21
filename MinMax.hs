--import Tp3.Ai
import Data.Maybe
import Data.List
import Data.Ord


minmax::(Ai a)=>a->action
minmax = tourMax
    where
        tourMin ai = fromMaybe (minimumBy (comparing snd)$ map tourMax $ transition ai) (but ai)
        tourMax ai = fromMaybe (maximumBy (comparing snd)$ map tourMin $ transition ai) (but ai)