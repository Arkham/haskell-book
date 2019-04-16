module Exercises18 where

import Control.Monad (join)

-- Nope Monad
data Nope a =
  NopeDotJpg

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

-- BahEither Monad
data BahEither a b
  = PLeft a
  | PRight b

instance Functor (BahEither e) where
  fmap _ (PLeft a) = PLeft a
  fmap fun (PRight b) = PRight (fun b)

instance Applicative (BahEither e) where
  pure = PRight
  (PLeft a) <*> _ = PLeft a
  (PRight fun) <*> value = fmap fun value

instance Monad (BahEither e) where
  return = pure
  (PLeft a) >>= _ = PLeft a
  (PRight value) >>= fun = fun value

-- Identity Monad
newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap fun (Identity a) = Identity (fun a)

instance Applicative Identity where
  pure = Identity
  (Identity fun) <*> (Identity a) = Identity (fun a)

instance Monad Identity where
  return = pure
  (Identity a) >>= fun = fun a

-- List Monad
data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap fun (Cons x xs) = Cons (fun x) (fmap fun xs)

instance Semigroup (List e) where
  xs <> Nil = xs
  Nil <> ys = ys
  (Cons x xs) <> ys = Cons x $ xs <> ys

instance Monoid (List e) where
  mempty = Nil

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons fun funs) <*> values = mappend (fmap fun values) (funs <*> values)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  (Cons x xs) >>= fun = mappend (fun x) (xs >>= fun)

-- Implement functions using methods in Monad and Functor
j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 fun ma mb = ma >>= \a -> mb >>= \b -> return (fun a b)

ap :: Monad m => m a -> m (a -> b) -> m b
ap ma mfun = do
  a' <- ma
  fun' <- mfun
  return (fun' a')

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) fun = do
  x' <- fun x
  xs' <- meh xs fun
  return (x' : xs')

flipType :: Monad m => [m a] -> m [a]
flipType xs = meh xs id
