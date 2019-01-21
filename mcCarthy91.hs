module McCarthy91 where

mc91 :: (Num a, Ord a) => a -> a
mc91 value
  | value > 100 = value - 10
  | otherwise = mc91 $ mc91 $ value + 11
