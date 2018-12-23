# Chapter 6 - Typeclasses

Typeclasses allow us to generalize over a set of types in order to define and
execute a standard set of features for those types. For example the ability to
test values for equality is useful and we'd want to be able to use that function
for data of various types.

- instance Bounded Bool – Bounded for types that have an upper and lower bound
- instance Enum Bool – Enum for things that can be enumerated
- instance Eq Bool – Eq for things that can be tested for equality
- instance Ord Bool – Ord for things that can be put into a sequential order
- instance Read Bool – Read parses strings into things.
- instance Show Bool – Show renders things into strings.

We’ve mentioned partial application of functions previously, but the term
partial function refers to something different. A partial function is one that
doesn’t handle all the possible cases, so there are possible scenarios in which
we haven’t defined any way for the code to evaluate.

### Hlint message: why?

```
derivingInstances.hs:34:1: Suggestion: Use newtype instead of data
Found:
  data Identity a = Identity a
Perhaps:
  newtype Identity a = Identity a
Note: decreases laziness
```

### Partial functions

A partial function is one that doesn’t handle all the possible cases, so there
are possible scenarios in which we haven’t defined any way for the code to
evaluate.

```
data DayOfWeek =
  Mon | Tue | Wed | Thu | Fri | Sat | Sun

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True

Prelude> Mon == Tue
*** Exception: code/derivingInstances.hs:
(19,3)-(25,23):
  Non-exhaustive patterns in function ==
```

```
*DerivingInstances> :set -Wall
*DerivingInstances> :l derivingInstances.hs
[1 of 1] Compiling DerivingInstances ( derivingInstances.hs, interpreted )

derivingInstances.hs:13:3: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for ‘==’:
        Patterns not matched:
            Mon Tue
            Mon Wed
            Mon Thu
            Mon Fri
            ...
Ok, modules loaded: DerivingInstances.
```

So does this mean you should always compile your code with `-Wall` enabled?

### LOL

Don’t use Int as an implicit sum type as C programmers commonly do.

### Num

```
Prelude> :info Num
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
```

### Integral

```
Prelude> :info Integral
class (Real a, Enum a) => Integral a where
  quot :: a -> a -> a
  rem :: a -> a -> a
  div :: a -> a -> a
  mod :: a -> a -> a
  quotRem :: a -> a -> (a, a)
  divMod :: a -> a -> (a, a)
  toInteger :: a -> Integer
  {-# MINIMAL quotRem, toInteger #-}
```

Any type that implements `Integral` must already have instances for `Real` and
`Enum`.

```
Prelude> :info Real
class (Num a, Ord a) => Real a where
  toRational :: a -> Rational
  {-# MINIMAL toRational #-}
```

The `Real` typeclass requires an instance of `Num`. So the `Integral` typeclass
may put the methods of `Real` and `Num` into effect.

Since `Real` cannot override the methods of `Num`, this typeclass inheritance is
only additive and the ambiguity problems caused by multiple inheritance in some
programming languages — the so-called "deadly diamond of death" — are avoided.

### Fractional

```
Prelude> :info Fractional
class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a
  {-# MINIMAL fromRational, (recip | (/)) #-}
```

This crashes:

```
Prelude> divideThenAdd x y = (x / y) + 1
Prelude> divideThenAdd :: Num a => a -> a -> a

<interactive>:37:1: error:
    • Could not deduce (Fractional a1)
        arising from a use of ‘divideThenAdd’
      from the context: Num a
        bound by the inferred type of it :: Num a => a -> a -> a
        at <interactive>:37:1-37
      or from: Num a1
        bound by an expression type signature:
                   Num a1 => a1 -> a1 -> a1
        at <interactive>:37:1-37
      Possible fix:
        add (Fractional a1) to the context of
          an expression type signature:
            Num a1 => a1 -> a1 -> a1
    • In the expression: divideThenAdd :: Num a => a -> a -> a
      In an equation for ‘it’: it = divideThenAdd :: Num a => a -> a -> a
```

While these work fine:

```
Prelude> subtractThenAdd x y = (x - y) + 1
Prelude> subtractThenAdd :: Num a => a -> a -> a

Prelude> divideThenAdd x y = (x / y) + 1
Prelude> divideThenAdd :: Fractional a => a -> a -> a
```

### Type-defaulting type classes

When you have a type class-constrained polymorphic value and need to evaluate
it, the polymorphism must be resolved to a concrete type.

But when you're working in the REPL, most often you don't specify a concrete
type for a polymorphic value. In those cases, the type class will default to a
concrete type. Here are some:

```
default Num Integer
default Real Integer
default Enum Integer
default Integral Integer
default Fractional Double
default RealFrac Double
default Floating Double
default RealFloat Double
```

```
Prelude> :t (+)
(+) :: Num a => a -> a -> a
Prelude> x = 5 + 5 :: Double
Prelude> :t x
x :: Double
```

We can go from general functions to specific functions

```
Prelude> add = (+) :: Integer -> Integer -> Integer
Prelude> add 10 10
20
```

But we cannot go from specific to general

```
Prelude> :t id
id :: a -> a
Prelude> numId = id :: Num a => a -> a
Prelude> intId = numId :: Integer -> Integer
Prelude> altNumId = intId :: Num a => a -> a

<interactive>:15:12: error:
    • Couldn't match type ‘a1’ with ‘Integer’
      ‘a1’ is a rigid type variable bound by
        an expression type signature:
          forall a1. Num a1 => a1 -> a1
        at <interactive>:15:21
      Expected type: a1 -> a1
        Actual type: Integer -> Integer
    • In the expression: intId :: Num a => a -> a
      In an equation for ‘altNumId’: altNumId = intId :: Num a => a -> a
```

We can't go back to `Num` because we lost its generality when we specialized to
`Integer`.

### Ord

```
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  {-# MINIMAL compare | (<=) #-}
```

```
compare True False
GT
```

You may notice that `True` is greater than `False`. This is due to how the
`Bool` datatype is defined: `False | True`. There might be also philosophical
implications.

```
data DayOfWeek =
  Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Ord, Show)
```

Values to the left are _less than_ values to the right.

We can also implement the instance ourselves:

```
instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _   = GT
  compare _   Fri = LT
  compare _   _   = EQ
```

But this leads to a somehow weird behaviour:

```
*OrdInstances> compare Fri Sat
GT
*OrdInstances> compare Sat Mon
EQ
*OrdInstances> Sat == Mon
False
```

It is generally wise to ensure that your `Ord` instances agree with your `Eq`
instances, wether the `Eq` instances are derived or manually written.

If we define a function like this

```
Prelude> check' :: a -> a -> Bool; check' a a' = a == a'
```

We would get this error

```
<interactive>:12:41: error:
    • No instance for (Eq a) arising from a use of ‘==’
      Possible fix:
        add (Eq a) to the context of
          the type signature for:
            check' :: a -> a -> Bool
    • In the expression: a == a'
      In an equation for ‘check'’: check' a a' = a == a'
```

And we could make the error go away with

```
Prelude> check' :: Eq a => a -> a -> Bool; check' a a' = a == a'
Prelude> check' 123 124
False
```

But we can also make the error go away with

```
Prelude> check' :: Ord a => a -> a -> Bool; check' a a' = a == a'
Prelude> check' 123 123
```

This is because we know that every type that is an instance of `Ord` must also
be an instance of `Eq`. We can say that `Eq` is a _superclass_ of `Ord`.

Since usually you want the minimally sufficient set of constraints on all your
functions, you would use `Eq` over `Ord` in real world code.

### Enum

This type class covers types that are enumerable.

```
Prelude> :info Enum
class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
  {-# MINIMAL toEnum, fromEnum #-}
```

```
Prelude> enumFromTo 3 8
[3,4,5,6,7,8]
Prelude> enumFromTo 3 8.5
[3.0,4.0,5.0,6.0,7.0,8.0,9.0]
Prelude> :t enumFromTo 3 8.5
enumFromTo 3 8.5 :: (Fractional a, Enum a) => [a]
Prelude> enumFromThenTo 1 10 100
[1,10,19,28,37,46,55,64,73,82,91,100]
Prelude> enumFromThenTo 'a' 'c' 'z'
"acegikmoqsuwy"
```

### Show

Show is the type class that provides for the creating of human readable string
representations of structured data. It's NOT a serialization format, it's only
for human readability.

```
Prelude> :info Show
class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
  {-# MINIMAL showsPrec | show #-}
```

Printing to screen is a side effect, so let's look at the type

```
Prelude> :t print
print :: Show a => a -> IO ()
```

The return value of that function is an `IO ()` result. This result is an `IO`
action that returns a value of the type `()`. An `IO` action is an that has side
effects, including reading from input and printing to the screen and will
contain a return value. The `()` denotes an empty tuple, which we refer as
_unit_. Unit is a value, and also a type that has only this one inhabitant, that
essentially represents nothing.

The simplest way to think about the difference between a value with a typical
type like `String` and one like `IO String` is that `IO` actions are formulas.
It's more of a _means of producing_ a `String`, which may require performing
side effects along the way before you get your `String` value.

### Read

```
class Read a where
  readsPrec :: Int -> ReadS a
  readList :: ReadS [a]
  GHC.Read.readPrec :: Text.ParserCombinators.ReadPrec.ReadPrec a
  GHC.Read.readListPrec :: Text.ParserCombinators.ReadPrec.ReadPrec
                             [a]
  {-# MINIMAL readsPrec | readPrec #-}
```

The problem with `Read` is that it takes strings and turns them into things.

`Prelude> :t read
read :: Read a => String -> a`

There is no way we can be guaranteed that we can ready any string and get
something back.

```
Prelude> read "1232321" ::Integer
1232321
Prelude> read "fdsa" :: Integer
*** Exception: Prelude.read: no parse
```

So in a way, `read` is a partial function, a function that doesn't return a
proper value as a result for all possible inputs.

### Instances are dispatched types


```
class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  defaultNumber :: a

newtype Age =
  Age Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber = Age
  toNumber (Age n) = n
  defaultNumber = Age 65
```

Why not write a type class like this? When we will talk about `Monoid`, it's
important that your type classes have laws and rules about how they work.
`Numberish` is a bit.. arbitrary. There are better ways to express what it does
in Haskell than a type class.

## Cool point

One of the nice things about parametricity and type classes is that you are
being explicit about what you mean to do with your data which means you are less
likely to make a mistake. `Int` is a big datatype with many inhabitants and many
type classes and operations defined for it — it could be easy to make a function
that does something unintended. Whereas if we were to write a function, even if
we had `Int` values in mind for it, which used a polymorphic type constrained by
the type class instances we wanted, we could ensure we only used the operations
we intended.

### Definitions

- _Type class inheritance_ is when a type class has a superclass. This is a way
  of expressing that a type class requires _another_ type class to be available
  for a given type before you can write an instance.
- _Effects_ are how to we refer to _observable_ actions programs may take other
  than compute a value. If a function modifies some state or interacts with the
  outside world in a manner that can be observed, then we say it has an _effect_
  on the world.
- `IO` is the type for values whose evaluation bears the possibility of causing
  side effects, such as printing text, reading text input from the user, reading
  and writing to files, or connecting to remote computers.
- An _instance_ is the definition of how a type class should work for a given
  type. Instances are unique for a given combination of type class and type.
