module Ai (Ai, transition, actions, goal, heuristic) where

class (Show a)=>Ai a where
   transition :: a->[a]
   actions :: a->[(Action,a)]
   goal :: a->(Maybe Integer)
   heuristic :: a->Integer

type Action = String
