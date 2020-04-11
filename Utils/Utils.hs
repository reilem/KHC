
module Utils.Utils where

import Data.List (nub, sort)
import Utils.PrettyPrint

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

partition :: PrettyPrint a => [(a -> Bool)] -> [a] -> [[a]]
partition fs xs = go fs fs xs
  where
    go :: PrettyPrint a => [(a -> Bool)] -> [(a -> Bool)] -> [a] -> [[a]]
    go _      _   []  = []
    go (g:gs) gss (a:as)
      | g a           = let (i, o) = part g (a:as) in (i : go gss gss o)
      | otherwise     = go gs gss (a:as)
    go _      _    as  = [panic ("No functions were provided." ++
      " Or no function was able to match the content in 'partition'." ++
      (render (ppr as)))]

part :: (a -> Bool) -> [a] -> ([a], [a])
part _ []     = ([], [])
part f (x:xs)
  | f x       = let (i, o) = part f xs in (x : i, o)
  | otherwise = ([], (x:xs))

-- Partition adjacent elements based on equality to return of given function
-- E.g. partition odd [1,3,2,4,1] = [[1,3],[2,4],[1]]
-- partition :: (Eq b) => (a -> b) -> [a] -> [[a]]
-- partition _ []       = []
-- partition _ [x]      = [[x]]
-- partition f (x:y:xs)
--   | f x == f y = tack x (partition f (y : xs))
--   | otherwise  = [x] : partition f (y : xs)

-- Checks if two (unordered) lists contain the same elements
-- E.g. listsEqual [3, 2, 1] [2, 1, 3] = True
-- E.g. listsEqual [5, 5] [4, 4] = False
areOrPatBindsEqual :: Ord a => [a] -> [a] -> Bool
areOrPatBindsEqual xs ys = distinct xs && distinct ys &&
  (sort xs == sort ys)

-- | Gives every possible combination from the list of lists
cart :: [[a]] -> [[a]]
cart []       = pure []
cart (xs:xss) = concat [[(x:ys) | ys <- cart xss] | x <- xs]
