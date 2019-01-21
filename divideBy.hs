module DivideBy where

data DividedResult =
  Result Integer
    | DividedByZero
    deriving Show

divideBy :: Integer -> Integer -> DividedResult
divideBy num denom
  | denom == 0 = DividedByZero
  | num >= 0 && denom > 0 = Result $ positiveDivideBy num denom
  | num < 0 && denom > 0 = Result $ negate $ positiveDivideBy (negate num) denom
  | num >= 0 && denom < 0 = Result $ negate $ positiveDivideBy num (negate denom)
  | num < 0 && denom < 0 = Result $ positiveDivideBy (negate num) (negate denom)

positiveDivideBy :: Integer -> Integer -> Integer
positiveDivideBy num denom = go num denom 0
  where go n d count
          | n >= d = go (n - d) d (count + 1)
          | otherwise = count
