module MySTD where

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs)
  | x = myAnd xs
  | otherwise = False

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs)
  | x = True
  | otherwise = myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny fn list =
  myOr $ map fn list

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem elem (x:xs)
  | elem == x = True
  | otherwise = myElem elem xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' elem = myAny (== elem)

myReverse :: [a] -> [a]
myReverse list = go list []
  where go [] acc = acc
        go (x:xs) acc = go xs (x : acc)

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap fn list = go list []
  where go [] acc = acc
        go (x:xs) acc = go xs (acc ++ fn x)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy compareFn list = go list (head list)
  where go [] max = max
        go (x:xs) max =
          if compareFn x max == GT
             then go xs x
             else go xs max


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy compareFn list = go list (head list)
  where go [] min = min
        go (x:xs) min =
          if compareFn x min == LT
             then go xs x
             else go xs min

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
