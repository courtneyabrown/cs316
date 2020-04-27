{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import Prelude hiding (words)
import Test.QuickCheck (quickCheck, (==>), Property)

{- This file includes some basic Haskell functions taken 
from Courtney Brown's CS316 coursework. -}

{- Concatenation of lists. -}

concatLists :: [[x]] -> [x]
concatLists []     = []
concatLists (x:xs) = x ++ concatLists xs

{- Splitting of lists. -}

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn splitChar xs = splitOnHelper splitChar [] xs

splitOnHelper :: Eq a => a -> [a] -> [a] -> [[a]]
splitOnHelper s group [] = [group]
splitOnHelper s group (x:xs)
                        | s == x = [group] ++ splitOnHelper s [] xs
                        | otherwise = splitOnHelper s (group ++ [x]) xs

{- Elements of a tree.
   Here is a datatype of binary trees, with data of type 'a' stored at
   the nodes: -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

{- Returns all the elements in a tree, in left to right order. -}

elements :: Tree a -> [a]
elements Leaf         = []
elements (Node l x r) = elements l ++ [x] ++ elements r


{- Finding things in trees. -}

eqFindInTree :: Eq a => a -> Tree a -> Bool
eqFindInTree find Leaf            = False
eqFindInTree find (Node l x r)  
                      | find == x = True
                      | find /= x = eqFindInTree find (l) || eqFindInTree find (r)
                      | otherwise = False


{- Removing empty lists from a list of lists. -}

removeEmpties :: [[a]] -> [[a]]
removeEmpties []                 = []
removeEmpties (l:list)
              | length l == 0    = removeEmpties list
              | length list == 0 = [l]
              | otherwise        = [l] ++ removeEmpties list


{- Discarding (Opposite of filter). -}

discard :: (a -> Bool) -> [a] -> [a]
discard p [] = []
discard p (x:xs)
            | p x       = filter (not . p) xs
            | otherwise = x : filter (not . p) xs

{- Sum: -}

summ :: [Double] -> Double
summ = foldr (+) 0

{- Len: -}

len :: [a] -> Integer
len = foldr (const (1+)) 0

{- Putting these together, we can calculate average: -}

avg :: [Double] -> Double
avg xs = summ xs / fromInteger (len xs)

{- More efficient version of sum and len. It has a single pass that
   computes the sum and length simulatenously and returns a pair:  -}

sumAndLen :: [Double] -> (Double, Integer)
sumAndLen = foldr f (0,0)
  where f x (total, length) = (total + x, length + 1) 

{- Using sumAndLen, this alternative average function works: -}

avg' :: [Double] -> Double
avg' xs = total / fromInteger length
  where (total, length) = sumAndLen xs

{- A 'Comparator' in Haskell is a function that takes two values and
   returns an 'Ordering' (satisfying some properties). Let's make a
   type synonym for Comparators: -}

type Comparator a = a -> a -> Ordering

{- Inverting Comparators. -}

invertOrdering :: Ordering -> Ordering
invertOrdering LT = GT
invertOrdering EQ = EQ
invertOrdering GT = LT

{-  Reverses ordering. -}

invert :: Comparator a         -> Comparator a
       -- (a -> a -> Ordering) -> (a -> a -> Ordering)
invert cmp x y = invertOrdering (cmp x y)

{- Sorting with a Comparator. -}

mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = merge (mergeSort xs1) (mergeSort xs2)
  where (xs1, xs2) = split xs [] []

split :: [a] -> [a] -> [a] -> ([a],[a])
split []       ys zs = (ys, zs)
split [x]      ys zs = (x:ys, zs)
split (y:z:xs) ys zs = split xs (y:ys) (z:zs)

merge :: Ord a => [a] -> [a] -> [a]
merge []     ys     = ys
merge xs     []     = xs
merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys) else y : merge (x:xs) ys

{- Rewritten 'mergeSort' so that it takes as input a 'Comparator a', 
instead of relying on the default one from the 'Ord' instance. -}

mergeSortWith :: Comparator a -> [a] -> [a]
mergeSortWith cmp []  = []
mergeSortWith cmp [x] = [x]
mergeSortWith cmp xs  = mergeWith cmp (mergeSortWith cmp xs1) (mergeSortWith cmp xs2)
  where (xs1, xs2)    = split xs [] []

mergeWith :: Comparator a -> [a] -> [a] -> [a]
mergeWith cmp [] ys         = ys
mergeWith cmp xs []         = xs
mergeWith cmp (x:xs) (y:ys) = if cmp x y == LT then x : mergeWith cmp xs (y:ys) else y : mergeWith cmp (x:xs) ys
