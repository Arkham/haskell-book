module ExercisesTypeclasses where

import Data.List (sort)

-- multiple choices answers: c a a c a

x :: Int -> Int
x blah =
  blah + 20

printIt :: IO ()
printIt =
  -- putStrLn (show x)
  print $ x 5

-- data Person = Person Bool
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson =
  print

-- data Mood = Blah | Woot deriving Show
data Mood = Blah | Woot deriving (Show, Eq)

settleDown :: Mood -> Mood
settleDown x =
  if x == Woot
     then Blah
     else x

type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

data Rocks =
  Rocks String
  deriving (Eq, Show)

data Yeah =
  Yeah Bool
  deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)

-- phew = Papu "chases" True -- no, have to be wrapped

truth = Papu (Rocks "chomsky") (Yeah True) -- all good

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p' -- all good

-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p' -- does not have an instance of Ord Papu


-- match the types

-- i :: Num a => a
-- i = 1
-- i :: a -- No instance for (Num a)

-- f :: Float
-- f = 10.0
-- f :: Num a => a -- Should be (Fractional a)

-- f :: Float
-- f = 1.0
-- f :: RealFrac a => a -- All good!

-- freud :: a -> a
-- freud x = x
-- freud :: Ord a => a -> a -- All good, we are just not using anything

-- freud' :: a -> a
-- freud' x = x
-- freud' :: Int -> Int -- All good, not using the concrete type

myX = 1 :: Int

-- sigmund :: Int -> Int
-- sigmund x = myX
-- sigmund :: a -> a -- Fail, expected type `a` got type `Int`

-- sigmund' :: Int -> Int
-- sigmund' x = myX
-- sigmund' :: Num a => a -> a -- Could not match type `Num a => a` got type `Int`

-- jung :: Ord a => [a] -> a
-- jung xs = head (sort xs)
-- jung :: [Int] -> Int -- All good, Int has a type instance of Ord

-- young :: [Char] -> Char
-- young xs = head (sort xs)
-- young :: Ord a => [a] -> a -- All good, same as before `Ord a` and `Char` behave the same

-- mySort :: [Char] -> [Char]
-- mySort = sort
-- -- signifier :: [Char] -> Char
-- signifier xs = head (mySort xs)
-- signifier :: Ord a => [a] -> a -- Cannot match type `Ord a => a` with `Char`

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk fn a b =
  fn a == b

arith :: Num b
      => (a -> b)
      -> Integer
      -> a
      -> b
arith fn n a =
  fn a + fromInteger n
