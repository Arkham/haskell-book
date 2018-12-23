-- {-# LANGUAGE NoMonomorphismRestriction #-}
module DetermineTheType where

example = 1

exampleA :: Num a => a
exampleA = (* 9) 6

exampleB = head [(0, "dodge"), (1, "kitten")]
-- exampleB :: Num a => (a, [Char])

exampleC = head [(0 :: Integer, "doge"), (1, "kitten")]
-- exampleC :: (Integer, [Char])

exampleD = if False then True else False
-- exampleD :: Bool

exampleE = length [1..5]
-- exampleE :: Int

exampleF = length [1..5] > length "TACOCAT"
-- exampleF :: Bool

exampleG = y * 10
  where x = 5
        y = x + 5
-- exampleG :: Num a => a

exampleH y = y * 10
  where x = 5
        y = x + 5
-- exampleH :: Num a => t -> a

exampleI = 4 / y
  where x = 5
        y = x + 5
-- exampleI :: Fractional a => a

exampleJ = x ++ y ++ z
  where x = "Julie"
        y = "<3"
        z = "Haskell"
-- exampleJ :: [Char]

functionH (x:_) = x
-- functionH :: [a] -> a

functionC x y =
  if (x > y) then True else False
-- functionC :: (Ord a) => a -> a -> Bool

functionS (x, y) = y
-- functionS :: (a, b) -> b

myFunc :: (x -> y)
       -> (y -> z)
       -> c
       -> (a, x)
       -> (a, z)
myFunc xToY yToZ _ (a, x) =
  (a, yToZ (xToY x))

i :: a -> a
i foo = foo

c :: a -> b -> a
c first second = first

c'' :: b -> a -> b
c'' = c

r :: [a] -> [a]
r [] = []
r list = list ++ list

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a =
  bToC (aToB a)

aa :: (a -> c) -> a -> a
aa aToc a =
  a

ab :: (a -> b) -> a -> b
ab aToB =
  aToB
