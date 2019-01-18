module Fibonacci where

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n
  | n < 0 = 0
  | otherwise = (fibonacci (n - 1)) + (fibonacci (n - 2))
