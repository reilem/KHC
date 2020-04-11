
module Utils.Utils where

import Data.List (nub, sort)

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
