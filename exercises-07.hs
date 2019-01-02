module ExercisesMoreFunctional where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where (xLast, _) = x `divMod` 10
        (_, d)     = xLast `divMod` 10

hunsDigit :: Integral a => a -> a
hunsDigit x = d
  where (xLast, _) = x `divMod` 100
        (_, d)     = xLast `divMod` 10

foldBool :: a -> a -> Bool -> a
foldBool x x' cond
  | cond = x'
  | otherwise = x

foldBool2 :: a -> a -> Bool -> a
foldBool2 x x' cond =
  case cond of
    True -> x'
    False -> x

-- why foldbool takes else return value first? is it because left is left and right is right?

g :: (a -> b) -> (a, c) -> (b, c)
g fn (a, c) =
  (fn a, c)
