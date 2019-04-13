module ConstantInstance where

newtype Constant a b = Constant
  { getConstant :: a
  } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ x = x

instance Monoid a => Applicative (Constant a) where
  pure = Constant a
  (<*>) _ a b = b
