module EnumFromTo where

eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool True True = [True]
eftBool False True = [False, True]
eftBool True False = []

myEnumFromTo :: (Ord a, Enum a) =>  a -> a -> [a]
myEnumFromTo from to = reverse $ go from to []
  where go from to acc
          | from > to = []
          | from == to = from : acc
          | from < to = go (succ from) to (from : acc)

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = myEnumFromTo

eftInt :: Int -> Int -> [Int]
eftInt = myEnumFromTo

eftChar :: Char -> Char -> String
eftChar = myEnumFromTo
