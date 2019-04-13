module IdentityInstance where

newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap fun (Identity el) = Identity $ fun el

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity fun) (Identity a) = Identity $ fun a
