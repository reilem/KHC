
module Utils.Utils where

import Data.List (nub, delete)

-- | Zip two lists into a list of tuples. Fail if lengths don't match.
zipExact :: [a] -> [b] -> [(a,b)]
zipExact = zipWithExact (,)

-- | Zip two lists into a list using a combining function. Fail if lengths don't match.
zipWithExact :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithExact _f []     []     = []
zipWithExact  f (x:xs) (y:ys) = f x y : zipWithExact f xs ys
zipWithExact _f _      _      = error "zipWithExact: length mismatch"

distinct :: Eq a => [a] -> Bool
distinct xs = nub xs == xs

notImplemented :: String -> a
notImplemented e = error ("Not Implemented: " ++ e)

panic :: String -> a
panic = error . (++) "Panic! Something went terribly wrong. "

-- Add element to front of first list in the list of lists
-- E.g. tack 1 [[2],[3,4]] = [[1,2],[3,4]]
tack :: a -> [[a]] -> [[a]]
tack x xs = (x : (head xs)) : (tail xs)

-- Parition adjacent elements based on equality to return of given function
-- E.g. partition odd [1,3,2,4,1] = [[1,3],[2,4],[1]]
partition :: (Eq b) => (a -> b) -> [a] -> [[a]]
partition _ []       = []
partition _ [x]      = [[x]]
partition f (x:y:xs)
  | f x == f y = tack x (partition f (y : xs))
  | otherwise  = [x] : partition f (y : xs)

-- Checks if two (unordered) lists contain the same elements
-- E.g. listsEqual [3, 2, 1] [2, 1, 3] = True
-- E.g. listsEqual [5, 5] [4, 4] = False
listsEqual :: Eq a => [a] -> [a] -> Bool
listsEqual []     []    = True
listsEqual []     (_:_) = False
listsEqual (x:xs) ys    = let ys' = delete x ys in
  (length ys /= length ys') && listsEqual xs ys'
