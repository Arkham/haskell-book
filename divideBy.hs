module DivideBy where

divideBy :: Integral a => a -> a -> (a, a)
divideBy num denom = go num denom 0
  where go n d count
          | n >= d = go (n - d) d (count + 1)
          | otherwise = (count, n)
