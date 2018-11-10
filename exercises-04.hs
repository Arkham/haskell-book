module Exercises where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x =
  reverse x == x

myAbs :: Integer -> Integer
myAbs num =
  if num > 0 then
             num
             else
             negate num

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
-- f (a, b) (c, d) =
--   ((b, d), (a, c))
f fstTuple sndTuple =
  ((fstTupleSnd, sndTupleSnd), (fstTupleFst, sndTupleFst))
    where fstTupleFst = fst fstTuple
          fstTupleSnd = snd fstTuple
          sndTupleFst = fst sndTuple
          sndTupleSnd = snd sndTuple

myAdd = (+)
lengthPlusOne :: [ a ] -> Int
lengthPlusOne xs = w `myAdd` 1
  where w = length xs

myId :: a -> a
myId = (\x -> x)

getFst :: (a, b) -> a
getFst (a, b) =
  a

