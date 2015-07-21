module Tp3.Ai (Ai) where

class (Show a, Ord a)=>Ai a where
    transition :: a->[(action,a)]
    goal :: a->(Maybe Int)
    heuristic :: a->Int