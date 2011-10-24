module Zeno
where

import Puzzle

data (Num a) => Zeno a = Zeno a deriving (Eq,Show)

instance (Num a,Ord a) => Puzzle (Zeno a) where
  solved (Zeno i) = i > 5
  choices (Zeno i) = [Zeno (i+1)]

data (Ord a) => BackSort a = BackSort [a] [a] deriving (Eq,Show)

makeBackSort :: (Ord a) => [a] -> BackSort a
makeBackSort = BackSort []

instance (Ord a) => Puzzle (BackSort a) where
  solved (BackSort _ []) = True
  solved (BackSort _ _) = False
  choices (BackSort [] xs) = [BackSort [x] [w | w <- xs, w /= x] | x <- xs]
  choices (BackSort (s:ss) xs) = [ BackSort (x:s:ss) [w | w <- xs, w /= x]
    | x <- xs, x < s]
