{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Int

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany Int8 where
  tooMany n = n > 42

newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)

newtype NamedGoats =
  NamedGoats (Int, String) deriving (Eq, Show)

instance TooMany NamedGoats where
  tooMany (NamedGoats (n, string)) = tooMany n

instance TooMany (Int, Int) where
  tooMany (n1, n2) = tooMany (n1 + n2)

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (a, b) = tooMany a && tooMany b
