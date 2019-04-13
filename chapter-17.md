# Applicative

`Monoid` gives us a means of mashing two values of the same type together.
`Functor` is for function application over some structure.

`mappend` smashes the structures together, when you do it on two lists,
they become one list. `fmap` applies a function to a value that is within
some structure while leaving that structure unaltered.

`Applicative` are monoidal functors. WTF? This type class allows for
function application lifterd over structure, but in this case even the
function we are applying is embedded in some structure. So, since both the
function and the value have some structure, it involves both monoids and
functors.

## Defining Applicative

```
Prelude> :info Applicative
class Functor f => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  GHC.Base.liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
        -- Defined in ‘GHC.Base’
instance Applicative (Either e) -- Defined in ‘Data.Either’
instance Applicative [] -- Defined in ‘GHC.Base’
instance Applicative Maybe -- Defined in ‘GHC.Base’
instance Applicative IO -- Defined in ‘GHC.Base’
instance Applicative ((->) a) -- Defined in ‘GHC.Base’
instance Monoid a => Applicative ((,) a) -- Defined in ‘GHC.Base’
```

Every type that has an `Applicative` instance must also have a `Functor`
instance. The `pure` function is very boring: it lifts something into an
applicative structure. The more interesting operation is `<*>`, aka apply,
or ap, or tie fighter.

Let's compare it with fmap

```
Prelude> :t (<$>)
(<$>) :: Functor f => (a -> b) -> f a -> f b

Prelude> :t (<*>)
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
```

The only difference is the `f` being applied to our `a -> b` function.

The `Control.Applicative` library provides some other convenience functions
like `liftA`, `liftA2` and `liftA3`

```
Prelude Control.Applicative> :t liftA
liftA :: Applicative f => (a -> b) -> f a -> f b

Prelude Control.Applicative> :t liftA2
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c

Prelude Control.Applicative> :t liftA3
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
```

Now if you look at `liftA` that's exactly like `fmap` only with
`Applicative` instead of `Functor`.

## Functor vs Applicative

Let's look at them:

```
fmap  ::   (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b
```

Also we can prove that

```
fmap f x = pure f <*> x
```

Let's try with an array

```
Prelude Control.Applicative> fmap (+1) [1, 2, 3]
[2,3,4]
Prelude Control.Applicative> pure (+1) <*> [1, 2, 3]
[2,3,4]
```

We can think of `pure` as a way to embedding a value of any type in the
structure that we are working with:

```
Prelude Control.Applicative> pure 1 :: [Int]
[1]

Prelude Control.Applicative> pure 1 :: Maybe Int
Just 1

Prelude Control.Applicative> pure 1 :: Either a Int
Right 1

Prelude Control.Applicative> pure 1 :: ([a], Int)
([],1)
```

## Applicative functors are monoidal functors

Let's look at these operators

```
 ($)  ::   (a -> b) -> a -> b
(<$>) ::   (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b
```

We know that `$` is a do-nothing infix function that only gives the
right-hand side more precedence.
When we look at `fmap` we are lifting our `a -> b` over the `f` wrapped
around the value and applying it.
When we get at `<*>` our function is also wrapped in the functorial
structure. Now we get to the _monoidal_ in monoidal functor.

`:: f (a -> b) -> f a -> f b`

If `f` is a type with a `Monoid` instance, then it's easy

`mappend :: Monoid a => a -> a -> a`

So with `Applicative` we have a `Monoid` for our structure and function
application for our values

```
mappend :: f          -> f   -> f
$       :: (a -> b)   -> a   -> b
(<*>)   :: f (a -> b) -> f a -> f b
```

So in a sense, we are enriching function application with the very
structure we were previously mapping over with `Functor`.

```
Prelude> [(*2), (*3)] <*> [4, 5]
[8,10,12,15]

-- [2 * 4, 2 * 5, 3 * 4, 3 * 5 ]

Prelude> [(+2), (*3), (/2)] <*> [4, 5]
[6.0,7.0,12.0,15.0,2.0,2.5]

-- [2 + 4, 2 + 5, 3 * 4, 3 * 5, 4 / 2, 5 / 2 ]
```

So in this case we enriched our function with "list-ness".
Same applies to maybes:

```
Prelude> Just (*2) <*> Just 2
Just 4

Prelude> Just (*2) <*> Nothing
Nothing

Prelude> Nothing <*> Just 2
Nothing
```

With `Maybe`, the ordinary functor is mapping over the possibility of a
value's nonexistence. With `Applicative`, even the function might not be
provided.

## Show me the monoids

If we think at this

```
Prelude> fmap (+1) ("blah", 0)
("blah",1)
```

This works because from `:info (,)` we can see

```
instance Functor ((,) a) where
  fmap f (x,y) = (x, f y)
```

But if we look further we can see this:

```
instance Monoid a => Applicative ((,) a)
instance (Monoid a, Monoid b) => Monoid (a, b)
```

So for the `Applicative` instance of the two-tuple, we don't need a
`Monoid` for the `b` because we are using function application to produce
the `b`. But we still need `Monoid` for the first value of the tuple!

```
Prelude> ("Woo", (+1)) <*> (" Hoo!", 0)
("Woo Hoo!",1)

Prelude Data.Monoid> (Sum 2, (+1)) <*> (Sum 0, 0)
(Sum {getSum = 2},1)

Prelude Data.Monoid> (Product 3, (+9)) <*> (Product 2, 8)
(Product {getProduct = 6},17)

Prelude Data.Monoid> (All True, (+1)) <*> (All False, 0)
(All {getAll = False},1)
```

## Maybes


```
instance Semigroup a => Semigroup (Maybe a) where
    Nothing <> b       = b
    a       <> Nothing = a
    Just a  <> Just b  = Just (a <> b)

instance Applicative Maybe where
    pure = Just

    Just f  <*> m       = fmap f m
    Nothing <*> _       = Nothing
```

## Applicatives in use

Let's look at the specialized type:

```
(<*>) :: f (a -> b) -> f a -> f b
-- given f = []
(<*>) :: [] (a -> b) -> [] a -> [] b
-- or
(<*>) :: [(a -> b)] -> [a] -> [b]
```

or in the repl

```
Prelude> :set -XTypeApplications

Prelude> :type (<*>) @[]
(<*>) @[] :: [a -> b] -> [a] -> [b]

Prelude> :type pure @[]
pure @[] :: a -> [a]
```

Let's apply it!

```
Prelude> [(+1), (*2)] <*> [2, 4]
[3,5,4,8]
```

It applies all the functions in order and returns a single list, not two
lists, nor a nested list. This is thanks to `Monoid`.

Let's look at another example:

```
-- fmap to build tuples
Prelude> ((,) 1) <$> [2,3]
[(1,2),(1,3)]

Prelude> (,) <$> [1, 2] <*> [3,4]
[(1,3),(1,4),(2,3),(2,4)]
```

In this case we `fmap` to build a list of functions that build tuples, then
`<*>` over our list `[3, 4]`. The result is a list composed of four tuples.
Another way to write this is using `liftA2`

```
Prelude Control.Applicative> liftA2 (,) [1, 2] [3, 4]
[(1,3),(1,4),(2,3),(2,4)]

Prelude Control.Applicative> liftA2 (+) [1, 2] [3, 4]
[4,5,5,6]

Prelude Control.Applicative> liftA2 max [1, 2] [3, 4]
[3,4,3,4]
```

## Identity

The `Identity` type is a way to introduce structure without changing the
semantics of what you're doing.

```
Prelude> import Data.Functor.Identity

Prelude Data.Functor.Identity> :t (<*>) @Identity
(<*>) @Identity :: Identity (a -> b) -> Identity a -> Identity b

Prelude Data.Functor.Identity> :t pure @Identity
pure @Identity :: a -> Identity a
```

Why would we need this?

```
Prelude Data.Functor.Identity> xs = [1, 2, 3]
Prelude Data.Functor.Identity> xs' = [9, 9, 9]

Prelude Data.Functor.Identity> const 1 2
1
Prelude Data.Functor.Identity> const 1 100
1

Prelude Data.Functor.Identity> const <$> xs <*> xs'
[1,1,1,2,2,2,3,3,3]

Prelude Data.Functor.Identity> mkId = Identity
Prelude Data.Functor.Identity> const <$> mkId xs <*> mkId xs'
Identity [1,2,3]
```

## Constant

The `Constant` type sort of throws away the function application.

```
instance (Monoid a) => Applicative (Constant a) where
    pure _ = Constant mempty
    {-# INLINE pure #-}
    Constant x <*> Constant y = Constant (x `mappend` y)
```

And this is how it works

```
Prelude> import Data.Functor.Constant
Prelude Data.Functor.Constant> import Data.Monoid
Prelude Data.Functor.Constant Data.Monoid> f = Constant (Sum 1)
Prelude Data.Functor.Constant Data.Monoid> g = Constant (Sum 2)
Prelude Data.Functor.Constant Data.Monoid> f <*> g
Constant (Sum {getSum = 3})
Prelude Data.Functor.Constant Data.Monoid> pure 1 :: Constant String Int
Constant ""
```

## Applicative Laws

1. Identity

`pure id <*> v = v`

Functor has a similar identity law

```
id [1..5]
fmap id [1..5]
pure id <*> [1..5]
```

2. Composition

```
pure (.) <*> u <*> v <*> w =
  u <*> (v <*> w)
```

This means: composing our functions first and them applying them is the
same as applying the functions first and the composing them.

```
Prelude> pure (.) <*> [(+1)] <*> [(*2)] <*> [1,2,3]
[3,5,7]

Prelude> [(+1)] <*> ([(*2)] <*> [1,2,3])
[3,5,7]

pure (.) <*> Just (+1) <*> Just (*2) <*> Just 1
Just 3

Prelude> Just (+1) <*> (Just (*2) <*> Just 1)
Just 3
```

3. Homomorphism

A homomorphism is a structure-preserving map between two algebraic
structures.

```
pure f <*> pure x = pure (f x)
```

These lines of code should return the same result

```
pure (+1) <*> pure 1

pure ((+1) 1)

(+1) 1
```

Since the structure provided by `pure` isn't meaningful. Just like `fmap`
is a special type of function application that ignores a surrounding
structure, with applicative, even the applied function has a structure, so
those structures have to be monoidal and come together in some way.

```
Prelude> pure (+1) <*> pure 1 :: Maybe Int
Just 2

Prelude> pure ((+1) 1) :: Maybe Int
Just 2

Prelude> pure (+1) <*> pure 1 :: [Int]
[2]

Prelude> pure (+1) <*> pure 1 :: Either a Int
Right 2
```

4. Interchange

`u <*> pure y = pure ($ y) <*> u`

On the left of `<*>` there must always be a function embedded in a
structure, something like

```
Prelude> Just (+2) <*> pure 2
Just 4
```

The right hand side is harder to understand. Let's check the types

```
Prelude> :t ($)
($) :: (a -> b) -> a -> b

Prelude> :t ($ 2)
($ 2) :: Num a => (a -> b) -> b
```

We have created a function that wraps `y`, awaiting for a function to be
applied to it.

```
Prelude> ($ 2) (+1)
3
Prelude> ($ 2) (/1)
2.0
```

Let's try making the Applicative types more concrete:

```
mPure :: a -> Maybe a
mPure = pure

embed :: Num a => Maybe ((a -> b) -> b)
embed = mPure ($ 2)

mApply :: Maybe ((a -> b) -> b)
       -> Maybe  (a -> b)
       -> Maybe              b
mApply = (<*>)

myResult = mPure ($ 2) `mApply` Just (+2)
```

So basically the interchange law states that to apply a function wrapped in
a structure to a value wrapped in a structure is equivalent as creating a
structure that embeds the value and waits for a function and applying that
to the function wrapped in a structure. Pretty sick, eh?

## Property Testing Applicative

Come back to this later.

## ZipList Monoid

While the default mappend implementation on lists does this:

`[1, 2, 3] <> [4, 5, 6]`

`[1, 2, 3] ++ [4, 5, 6]`

`[1, 2, 3, 4, 5, 6]`

The `ZipList` monoid combines the values as parallel sequences

`[1, 2, 3] <> [4, 5, 6]`

```
[ 1 <> 4
, 2 <> 5
, 3 <> 6
]
```

## Definitions

`Applicative` can be thought of characterizing monoidal functors in
Haskell. It's a way to functorially apply a function which is embedded in
a structure to a value embedded in the same structure.
