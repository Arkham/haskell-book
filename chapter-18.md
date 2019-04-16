# Monad

Monads are applicative functors, but they have something that makes them
more powerful than `<*>` or `fmap` alone.

```
Prelude> :info Monad
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  fail :: String -> m a
  {-# MINIMAL (>>=) #-}
```

As you can see Applicative is a superclass of Monad. This means that Monad
is stronger than Applicative and Applicative is stronger than Functor. So
for example you can write `fmap` in terms of monadic operations!

`fmap f xs = xs >>= return . f`

See this

```
Prelude> fmap (+1) [1,2,3]
[2,3,4]

Prelude> [1,2,3] >>= return . (+1)
[2,3,4]
```

What is important to understand is this chain of dependency

`Functor -> Applicative -> Monad`

Let's look at the Monad three core operations:

```
(>>=) :: m a -> (a -> m b) -> m b
(>>) :: m a -> m b -> m b
return :: a -> m a
```

`return` is the same as `pure`, it takes a value and returns it inside a
structure. `>>` is sometimes referred as the sequencing operator, and it
sequences two actions while discarding any resulting value from the first
action.

Finally, let's look at `>>=`, this is called bind and is one of the reasons
why Monad is so special!

Let's look at the types of `fmap`, `<*>` and `>>=`

```
fmap :: Functor f
     => (a -> b) -> f a -> f b
<*>  :: Applicative f
     => f (a -> b) -> f a -> b
>>=  :: Monad f
     => f a -> (a -> f b) -> f b
```

If we look at `fmap` and use `f b` instead of `b` we obtain this:

```
fmap :: Functor f
     => (a -> f b) -> f a -> f (f b)
```

This is quite close to the signature of bind, no? Let's see an example

```
Prelude> andOne x = [x, 1]
Prelude> andOne 10
[10,1]

Prelude> :t fmap andOne [4, 5, 6]
fmap andOne [4, 5, 6] :: Num a => [[a]]

Prelude> fmap andOne [4, 5, 6]
[[4,1],[5,1],[6,1]]
```

We have a list of nested lists, but we could tell that beforehand by
looking at `f (f b)`. What if we wanted a way to discard that extra level
of structure that has been generated?

We could concat the result of the fmap

```
Prelude> concat $ fmap andOne [4, 5, 6]
[4,1,5,1,6,1]
```

In a way, Monad is a generalization of concat! Let's look at `join`

```
Prelude> :t Control.Monad.join
Control.Monad.join :: Monad m => m (m a) -> m a
```

If we concretize the type to lists that's what we get

```
Prelude> :t Control.Monad.join @[]
Control.Monad.join @[] :: [[a]] -> [a]
```

The ability to flatten these two layers of structure into one is what makes
`Monad` special. With `join` and `fmap` we can then write `bind`:

```
-- note that this is (>>=) flipped
bind :: Monad m => (a -> m b) -> m a -> m b
bind fun a =
  Control.Monad.join $ fmap fun a
```

## What monad is not

A monad is not:
- impure. Monadic functions are pure functions. `IO` is an abstract datatype
    that allows for impure actions and it has a `Monad` instance. There is
    nothing impure about monads.
- an embedded language for imperative programming
- a value
- about strictness
- math or category theory specific.

## Monad also lifts!

The `Monad` class also includes some lift functions such as:

```
Prelude> :t Control.Monad.liftM
Control.Monad.liftM :: Monad m => (a1 -> r) -> m a1 -> m r
```

which is very similar to

```
Prelude> :t Control.Applicative.liftA
Control.Applicative.liftA :: Applicative f => (a -> b) -> f a -> f b
```

which in turn was very similar to

```
Prelude> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
```

apart from the type class constraint.

See below for some examples:

```
Prelude> import Control.Applicative (liftA2)
Prelude Control.Applicative> import Control.Monad (liftM2)

Prelude Control.Applicative Control.Monad> liftA2 (,) (Just 2) (Just 3)
Just (2,3)

Prelude Control.Applicative Control.Monad> liftM2 (,) (Just 2) (Just 3)
Just (2,3)
```

## Do syntax and monads

Let's look at these two functions:

```
Prelude> :t (*>)
(*>) :: Applicative f => f a -> f b -> f b

Prelude> :t (>>)
(>>) :: Monad m => m a -> m b -> m b
```

They look very similar and they both are used to sequence functions.

```
Prelude> putStrLn "Hello, " >> putStrLn "World!"
Hello,
World!

Prelude> putStrLn "Hello, " *> putStrLn "World!"
Hello,
World!
```

Well, we could write the same with `do` notation

```
Prelude> :{
Prelude| do
Prelude|   putStrLn "Hello, "
Prelude|   putStrLn "World!"
Prelude| :}
Hello,
World!
```

We can do the same with variable binding

```
Prelude> :{
Prelude| do
Prelude|   name <- getLine
Prelude|   putStrLn name
Prelude| :}
Ju
Ju
```

To write the same without `do` we would have to write

```
Prelude> getLine >>= putStrLn
Ju
Ju
```

You may be wondering: why is `fmap` not enough?

```
Prelude> putStrLn <$> getLine
Ju
```

This does not print our name again. Why? Look at the types

```
Prelude> :t putStrLn
putStrLn :: String -> IO ()

Prelude> :t getLine
getLine :: IO String

Prelude> :t putStrLn <$> getLine
putStrLn <$> getLine :: IO (IO ())
```

As we can see, we have two much structure and we need to join those two
`IO` layers together. We can do that with `join`

```
Prelude> Control.Monad.join $ putStrLn <$> getLine
Ju
Ju
```

or just with bind!

If we wanted to re-write this function

```
getNameAndPrint :: IO ()
getNameAndPrint = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn $ "y helo thar: " ++ name
```

we would have to write something like this

```
getNameAndPrint :: IO ()
getNameAndPrint =
  putStrLn "name pls:" >>
  getLine >>=
  \name ->
    putStrLn $ "y helo thar: " ++ name
```

## Examples of Monad use

### List

Let's see the specialized functions:

```
Prelude> :t (>>=) @[]
(>>=) @[] :: [a] -> (a -> [b]) -> [b]

Prelude> :t return @[]
return @[] :: a -> [a]
```

So we would use it like this

```
Prelude> [1, 2, 3] >>= (\i -> [i, 2])
[1,2,2,2,3,2]

Prelude> [1, 2, 3] >>= return
[1,2,3]

Prelude> :{
Prelude| twiceWhenEven :: [Integer] -> [Integer]
Prelude| twiceWhenEven xs = do
Prelude|   x <- xs
Prelude|   if even x
Prelude|     then [x*x, x*x]
Prelude|     else [x*x]
Prelude| :}
Prelude> twiceWhenEven [1..4]
[1,4,4,9,16,16]
```

### Maybe

As usual, specialize the types:

```
Prelude> :t (>>=) @(Maybe)
(>>=) @(Maybe) :: Maybe a -> (a -> Maybe b) -> Maybe b

Prelude> :t return @(Maybe)
return @(Maybe) :: a -> Maybe a
```

```
data Cow = Cow
  { name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w >= 500
    then Nothing
    else Just c

mkSphericalCow :: String
               -> Int
               -> Int
               -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              weightCheck (Cow nammy agey weighty)
```

The last function can be rewritten as

```
mkSphericalCow :: String
               -> Int
               -> Int
               -> Maybe Cow
mkSphericalCow name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)
```

Or using `>>=`

```
mkSphericalCow :: String
               -> Int
               -> Int
               -> Maybe Cow
mkSphericalCow name' age' weight' = do
  noEmpty name' >>=
    \nammy ->
      noNegative age' >>=
        \agey ->
          noNegative weight' >>=
            \weighty ->
              weightCheck (Cow nammy agey weighty)
```

But we couldn't rewrite this using `Applicative` because our weightCheck
function depends of the prior existence of a `Cow` value.

The main thing to remember is:

- With the `Maybe Applicative`, each computation fails or succeeds
    independently of each other. You're lifting functions that are also
    `Just` or `Nothing` over `Maybe` values.
- With the `Maybe Monad`, computations contributing to the final result can
    choose to return `Nothing` based on previous computations.

It's interesting to see that the `Monad` implementation for `Maybe` looks
something like this:

```
instance Monad Maybe where
  return x = Just x

  (Just x) >>= k = k x
  Nothing >>= _ = Nothing
```

## Either

```
Prelude> :t (>>=) @(Either *)
(>>=) @(Either *) :: Either * a
                  -> (a -> Either * b)
                  -> Either * b

Prelude> :t return @(Either *)
return @(Either *) :: a -> Either * a
```

Let's see an example in `eitherMonad.hs`

## Monad laws

1. Identity

```
m >>= return =
  m

return x >>= f =
  f x
```

Basically this means that return should be neutral and not perform any
computation.

2. Associativity

```
(m >>= f) >>= g =
  m >>= (\x -> f x >>= g)
```

This looks a bit weird because of the order of the arguments of `>>=`.


## Property testing monads.

Come back to this later.

## Application and composition

Composition under functors just works

```
fmap id = id

fmap f . fmap g = fmap (f . g)

Prelude> fmap ((+1) . (+2)) [1..5]
[4,5,6,7,8]

Prelude> fmap (+1) . fmap (+2) $ [1..5]
[4,5,6,7,8]
```

Let's try writing the same for monads

```
mcomp :: Monad m => (b -> m c)
                 -> (a -> m b)
                 -> a
                 -> m c
mcomp f g a = f (g a)
```

This blows up because we g returns `m b` and we try to pass that to f. The
error message is:

```
Occurs check: cannot construct the infinite type: b ~ m b
```

We have to get rid of the extra layer of structure, so we can use `fmap`
and `join`

```
mcomp :: Monad m => (b -> m c)
                 -> (a -> m b)
                 -> a
                 -> m c
mcomp f g a = Control.Monad.join (f <$> (g a))
```

Or we could simply use `>>=`

```
mcomp :: Monad m => (b -> m c)
                 -> (a -> m b)
                 -> a
                 -> m c
mcomp f g a = g a >>= f
```

## Kleisli composition

Let's look at the types of `.` and `>>=`

```
Prelude> :t (.)
(.) :: (b -> c) -> (a -> b) -> a -> c

Prelude> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b

Prelude> import Control.Monad
Prelude Control.Monad> :t (>=>)
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
```

As you can see, `>=>` is basically the monadic version of `.` with the
arguments flipped. Let's see an example

```
sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge =
  getAge "Hello! How old are you? "
```

We used `return . read` to wrap the value in a monadic structure.

## Definition

`Monad` is a type class that describes a common behaviour. You are
functorially applying a function which produces more structure and then
using `join` to reduce the nested structure.

A monadic function is one which generates more structure. Compare

```
Prelude> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b

Prelude> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

Same difference can be seen when composing functions

```
Prelude Control.Monad> :t (.)
(.) :: (b -> c) -> (a -> b) -> a -> c

Prelude Control.Monad> :t (>=>)
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
```

Unfortunately `bind` is an overloaded term. When talking about monads, we
typically mean to lift a monadic function over the structure. So when using
do-notation `<-` or `>>=` we sometimes say "binding over".
