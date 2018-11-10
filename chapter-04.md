# Chapter 4 - Basic datatypes

Types are how we group a set of values together that share something in common.

```
data Bool = True | False
```

```
Prelude> :info Bool
data Bool = False | True        -- Defined in ‘GHC.Types’
instance Bounded Bool -- Defined in ‘GHC.Enum’
instance Enum Bool -- Defined in ‘GHC.Enum’
instance Eq Bool -- Defined in ‘GHC.Classes’
instance Ord Bool -- Defined in ‘GHC.Classes’
instance Read Bool -- Defined in ‘GHC.Read’
instance Show Bool -- Defined in ‘GHC.Show’
```

Defining your own data types:

```
data Mood = Blah | Woot deriving Show
```

```
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah 
```

## Number types
- Int
- Integer
- Float
- Double
- Rational
- Fractional
- Scientific (available in library)

```
Prelude> import GHC.Int
Prelude GHC.Int> 127 :: Int8
127
Prelude GHC.Int> 128 :: Int8

<interactive>:18:1: warning: [-Woverflowed-literals]
    Literal 128 is out of the Int8 range -128..127
    If you are trying to write a large negative literal, use NegativeLiterals
-128
```

You can find out the minimum and maximum bounds of numeric types using maxBound
and minBound from the Bounded typeclass. Here’s an example using our Int8 and
Int16 example:

```
Prelude> import GHC.Int
Prelude> :t minBound
minBound :: Bounded a => a
Prelude> :t maxBound
maxBound :: Bounded a => a
Prelude> minBound :: Int8
-128
Prelude> minBound :: Int16
-32768
```

```
Prelude GHC.Int> :t (/)
(/) :: Fractional a => a -> a -> a
```

The Fractional notation denotes a typeclass constraints. It tells us the type
variable a must implement the Fractional typeclass. Num is a superclass of
Fractional.

```
Prelude GHC.Int> :t (==)
(==) :: Eq a => a -> a -> Bool
Prelude GHC.Int> :t (<)
(<) :: Ord a => a -> a -> Bool
```

Eq is a typeclass that includes everything that can be compared
Ord is a typeclass that includes all things that can be ordered

```
Prelude> 'a' > 'b'
False
Prelude> "Julie" > "Chris"
True
Prelude> ['a', 'b'] > ['b', 'a']
False
Prelude> 1 > 2
False
Prelude> [1, 2] > [2, 1]
False

Prelude GHC.Int> "Julie" == 8

<interactive>:41:12: error:
    • No instance for (Num [Char]) arising from the literal ‘8’
    • In the second argument of ‘(==)’, namely ‘8’
      In the expression: "Julie" == 8
      In an equation for ‘it’: it = "Julie" == 8
```

```
Prelude GHC.Int> if True then 100 else undefined
100
Prelude GHC.Int> if False then 100 else undefined
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:54:24 in interactive:Ghci29
```

## Tuples

```
Prelude> :info (,)
data (,) a b = (,) a b  -- Defined in ‘GHC.Tuple’
instance (Bounded a, Bounded b) => Bounded (a, b)
  -- Defined in ‘GHC.Enum’
instance (Eq a, Eq b) => Eq (a, b) -- Defined in ‘GHC.Classes’
instance Monoid a => Monad ((,) a) -- Defined in ‘GHC.Base’
instance Functor ((,) a) -- Defined in ‘GHC.Base’
instance (Ord a, Ord b) => Ord (a, b) -- Defined in ‘GHC.Classes’
instance (Read a, Read b) => Read (a, b) -- Defined in ‘GHC.Read’
instance (Show a, Show b) => Show (a, b) -- Defined in ‘GHC.Show’
instance Monoid a => Applicative ((,) a) -- Defined in ‘GHC.Base’
instance Foldable ((,) a) -- Defined in ‘Data.Foldable’
instance Traversable ((,) a) -- Defined in ‘Data.Traversable’
instance (Monoid a, Monoid b) => Monoid (a, b)
  -- Defined in ‘GHC.Base’
```

A tuple is a product type, not a sum type.

## Definition

A typeclass is a set of operations defined with respect to a polymorphic type.
When a type has an instance of a typeclass, values of that type can be used in
the standard operations defined for that typeclass. In Haskell, typeclasses are
unique pairings of class and concrete instance: if a type `a` has an instance of
`Eq`, it has only one instance of `Eq`.

```
type Name = String

data Pet = Cat | Dog Name
```

`Pet` is a type constructor, not a value, and it can only be used in type
signatures.
