# Algebraic datatypes

What it means when we say that a datatype is algebraic?

A type can be thought of as an enumeration of constructors that have zero or
more arguments.

Haskell offers sum types, product types, product types with record syntax, type
aliases and a special datatype called `newtype`.

## Data and type constructors

```
data Bool = False | True

data [] a = [] | a : [a]
```

There are two types of constructors in Haskell, type constructors and data
constructors. Type constructors are used only at the type level, in type
signatures and type class declarations and instances. Types are static and
resolve at compile time. Data constructors construct the values at term level,
creating values you can interact with at runtime.

Type and data constructors that take no arguments are constants. For example,
`Bool` is  a type constant and it enumerates two values that are also constant,
`True` and `False`.

```
data Trivial = Trivial'

data UnaryTypeCon a = UnaryValueCon a
```

## Type constructors and kinds

Kinds are the types of types or types one level up. We represent them in Haskell
with `*`. We know that something has been fully applied, concrete type when we
write it as `*`. Instead, if it was a function, still waiting to be applied we
would write it as `* -> *`.

```
Prelude> :k Bool
Bool :: *

Prelude> :k [Int]
[Int] :: *

Prelude> :k []
[] :: * -> *
```

As we can see, the kind of `[]` is `* -> *` because it still needs to be applied
to a concrete type before it can become itself a concrete type.

## Data constructors and values

```
data PugType = PugData

data HuskyType a = HuskyData -- phantom!

data DogueDeBordeaux doge = DogueDeBordeaux doge
```

```
Prelude> :t DogueDeBordeaux
DogueDeBordeaux :: doge -> DogueDeBordeaux doge
```

```
data Doggies a =
  Husky a
  | Mastiff a
  deriving (Eq, Show)

Prelude> :k Doggies
Doggies :: * -> *

Prelude> :t Husky
Husky :: a -> Doggies a
```

```
data Price =
  Price Integer
  deriving (Eq, Show)

data Manufacturer =
  Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline =
  PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle =
  Car Manufacturer Price
  | Plane Airline
  deriving (Eq, Show)
```

## Data constructor arities

Arity refers to the number of arguments a function or a constructor takes. A
function that takes no arguments is called _nullary_. Same for data constructors
that take no arguments like `True` or `False`. Data constructors that take one
argument are called _unary_, while data constructors that take more than one
argument are called _products_.

```
data Nullary = Nullary

data Unary = Unary Int

data Product = Product Int String
```

An example of product is a tuple, which is actually an _anonymous product_.

## What makes these datatypes algebraic?

We can describe the pattern of argument structures using two basic operations:
sum and product. We can demonstrate these operations in terms of _cardinality_.

The cardinality of a datatype is the number of possible values it defines. The
number can be as small as 0 or as large as infinite.

For example, `Bool` has two possible values, so its cardinality is 2. `Int8`
describes whole numbers from -128 to 127. Its cardinality is therefore 256.

```
Prelude> import Data.Int
Prelude Data.Int> minBound :: Int8
-128
Prelude Data.Int> maxBound :: Int8
127
```

### Simple datatypes with nullary data constructors

```
data Example = MakeExample deriving Show
-- nullary constructor, cardinality is 1

data Goats = Goats Int deriving (Eq, Show)
-- unary constructor, cardinality is same as Int
```

## newtype

A `newtype` cannot be a product type, sum type or have nullary data
constructors. It has a few advantages over a vanilla `data` declaration:

- it has no runtime overhead. The difference between `newtype` and the type it
  contains is gone by the time the compiler generates the code.

```
tooManyGoats :: Int -> Bool
tooManyGoats n = n > 42
```

But in this case we could be mixing ints and passing a number that describes a
number of cows instead. What if we had this instead?

```
newtype Goats =
  Goats Int deriving (Eq, Show)

newtype Cows =
  Cows Int deriving (Eq, Show)
```

Now we can rewrite the function like this

```
tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42
```

and now we get this type error:

```
Prelude Data.Int> tooManyGoats (Goats 20)
False

Prelude Data.Int> tooManyGoats (Cows 20)

<interactive>:37:15: error:
    • Couldn't match expected type ‘Goats’ with actual type ‘Cows’
    • In the first argument of ‘tooManyGoats’, namely ‘(Cows 20)’
      In the expression: tooManyGoats (Cows 20)
      In an equation for ‘it’: it = tooManyGoats (Cows 20)
```

A `newtype` is similar to type synonyms in the sense that after the compilation
step the difference between the named value and the original value is gone.

However, you can do something with `newtype` that you cannot do with type
synonyms: you can define type class instances that differ from the instances for
their underlying type. Let's say we have this type class:

```
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42
```

Now let's say you want a special instance of `TooMany` for your goat counting.

```
newtype Goats = Goats Int deriving Show

instance TooMany Goats where
  tooMany (Goats n) = n > 43
```

And this is the difference compared with Ints:

```
Prelude Data.Int> tooMany (Goats 43)
False
Prelude Data.Int> tooMany (43 :: Int)
True
```

## Sum types

Let's look at a simple sum type:

```
data Bool = False | True
```

To know the cardinality of the sum type we add the cardinalities of their data
constructors.

### WTF Haskell

If you choose (-128) for a value precisely, you’ll notice you get a spurious
warning:

```
Prelude> n = Numba (-128)
Literal 128 is out of the
  Int8 range -128..127
If you are trying to write a large negative
  literal, use NegativeLiterals
```

Now, since -128 is a perfectly valid Int8 value you could choose to ignore this.
What happens is that (-128) desugars into (negate 128). The compiler sees that
you expect the type Int8, but Int8’s max boundary is 127. So even though you’re
negating 128, it hasn’t done that step yet and immediately whines about 128
being larger than 127.

## Product types

A product type's cardinality is the product of the cardinalities of its
inhabitants. The interesting fact about cardinality is that it roughly equates
to how difficult it is to reason about.

## Record syntax

Let's define a simple product type

```
data Person =
  MkPerson String Int
  deriving (Eq, Show)
```

if we used record syntax this is how it would look like

```
data Person =
  Person { name :: String
         , age :: Int }
         deriving (Eq, Show)
```

## Normal form

All the existing algebraic rules for products and sums apply in type systems as
well, and that includes the distributive property. For example we would agree
with

```
a * (b + c) -> (a * b) + (a * c)
```

Well, this applies to types too! So we can express a product of sum types as a sum of product types. For example if we had

```
data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

data BookType = FictionBook Fiction
              | NonfictionBook Nonfiction
              deriving Show
```

The arguments of `FictionBook` and `NonfictionBook` are the type constructors,
not the data constructors.

```
type AuthorName = String

data Author = Author (AuthorName, BookType)
```

This isn't the normal form, let's rewrite to be a sum of products

```
type AuthorName = String

data Author =
    Fiction_ AuthorName
  | Nonfiction_ AuthorName
  deriving (Eq, Show)
```

## Function type is exponential

Given a function `a -> b`, we can calculate the inhabitants with `b ^ a`

```
a -> b -> c

(c ^ b) ^ a

c ^ (b * a)
```

So a function `Bool -> Bool` has `2 * 2` inhabitants.

```
data Quantum =
  Yes
  | No
  | Both
  deriving (Eq, Show)
```

A function `Quantum -> Bool` has `2 * 3` inhabitants.

```
convert1 :: Quantum -> Bool
convert1 Yes = True
convert1 No = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes = True
convert2 No = True
convert2 Both = False

convert3 :: Quantum -> Bool
convert3 Yes = True
convert3 No = False
convert3 Both = True

convert4 :: Quantum -> Bool
convert4 Yes = True
convert4 No = True
convert4 Both = False

convert5 :: Quantum -> Bool
convert5 Yes = True
convert5 No = False
convert5 Both = False

convert6 :: Quantum -> Bool
convert6 Yes = False
convert6 No = True
convert6 Both = False

convert7 :: Quantum -> Bool
convert7 Yes = False
convert7 No = False
convert7 Both = True

convert8 :: Quantum -> Bool
convert8 Yes = False
convert8 No = False
convert8 Both = False
```

As you can see, there are exactly 8 possible implementations of this function.

## Higher Kinded datatypes

```
Prelude> :{
Prelude| data Silly a b c d =
Prelude|     MkSilly a b c d deriving Show
Prelude| :}

Prelude> :kind Silly
Silly :: * -> * -> * -> * -> *

Prelude> :kind Silly Int
Silly Int :: * -> * -> * -> *

Prelude> :kind Silly Int String
Silly Int String :: * -> * -> *

Prelude> :kind Silly Int String Bool
Silly Int String Bool :: * -> *

Prelude> :kind Silly Int String Bool String
Silly Int String Bool String :: *
```

Our datatype is very similar to a tuple with 5 elements:

```
Prelude> :kind (,,,,)
(,,,,) :: * -> * -> * -> * -> * -> *
```

## Binary Trees

See `./binaryTree.hs`

## As patterns

You can pattern match against a part of something and still refere to the
entire original value.

```
f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t
```

## Definitions

* A datatype is how we declare and create data for our functions. They begin with
the keyword `data` and are made up of a type constructor and zero or more data
constructors. Each data constructor may have zero or more arguments.
