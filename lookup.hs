module Lookup where

import Control.Applicative (liftA2)
import Data.List (elemIndex)

-- Prelude> lookup 1 [(1, "foo"), (2, "bar")]
-- Just "foo"
-- Prelude> lookup 3 [(1, "foo"), (2, "bar")]
-- Nothing
--
f :: Int -> Maybe String
f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]

g :: Int -> Maybe String
g x = lookup x [(7, "sup?"), (8, "chris"), (9, "aloha")]

h :: Int -> Maybe Int
h x = lookup x [(2, 3), (5, 6), (7, 8)]

m :: Int -> Maybe Int
m x = lookup x [(4, 10), (8, 13), (1, 9001)]

--
-- *Lookup> (++ "hi") <$> f 3
-- Just "hellohi"
-- *Lookup> (++) <$> f 3 <*> g 7
-- Just "hellosup?"
-- *Lookup> (++) <$> f 3 <*> g 10
-- Nothing
-- *Lookup> (++) <$> f 2 <*> g 10
-- Nothing
--
-- *Lookup> (+) <$> h 5 <*> m 1
-- Just 9007
-- *Lookup> (+) <$> h 5 <*> m 6
-- Nothing
--
-- *Lookup> Control.Applicative.liftA2 (+) (h 5) (m 1)
-- Just 9007
-- *Lookup> Control.Applicative.liftA2 (+) (h 5) (m 6)
-- Nothing
--
-- *Lookup> Control.Applicative.liftA2 (++) getLine getLine
-- foo
-- bar
-- "foobar"
-- *Lookup> Control.Applicative.liftA2 (,) getLine getLine
-- foo
-- nar
-- ("foo","nar")
--
added :: Maybe Int
added = fmap (+ 3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Int
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Int
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Int, Int)
-- tupled = liftA2 (,) y z
tupled = (,) <$> y <*> z

-- *Lookup> :t sum @((,) _)
-- sum @((,) _) :: Num a => (w, a) -> a
summed :: Maybe Int
-- summed = fmap sum $ liftA2 (,) y z
summed = sum <$> ((,) <$> y <*> z)

--
-- *Lookup> :t Data.List.elemIndex
-- Data.List.elemIndex :: Eq a => a -> [a] -> Maybe Int
--
x_ :: Maybe Int
x_ = elemIndex 3 [1 .. 5]

y_ :: Maybe Int
y_ = elemIndex 4 [1 .. 5]

max_ :: Int -> Int -> Int
max_ = max

maxed :: Maybe Int
-- maxed = liftA2 max_ x_ y_
maxed = max_ <$> x_ <*> y_
