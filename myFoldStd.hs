module MyFoldStd where

import Data.List (foldl')

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) =
  if x == False
     then False
     else myAnd xs

myAnd' :: [Bool] -> Bool
myAnd' [] = True
myAnd' (x:xs) = x && myAnd xs

myAnd'' :: [Bool] -> Bool
myAnd'' = foldr
  (\a b ->
    if a == False
       then False
       else b
  )
  True

myAnd''' :: [Bool] -> Bool
myAnd''' = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> f a || b) False

myElem :: Eq a => a -> [a] -> Bool
myElem elem =
  myAny (== elem)

myElem' :: Eq a => a -> [a] -> Bool
myElem' elem =
  foldr (\a b -> a == elem || b) False

myReverse :: [a] -> [a]
myReverse =
  foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f =
  foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f =
  foldr (\a b -> if f a then a : b else b) []

squish :: [[a]] -> [a]
squish =
  foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f =
  foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain =
  squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy compareFn list =
  foldl'
    (\a b ->
      if compareFn a b == LT
         then b
         else a
    )
    (head list)
    list

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy compareFn list =
  foldl'
    (\a b ->
      if compareFn a b == GT
         then b
         else a
    )
    (head list)
    list
