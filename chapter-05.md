# Chapter 5 - Types

Haskell is an implementation of a pure lambda calculus, i.e. there isn't much
more over the basic system of variables, abstractions and applications that
constitute the rules of a typed lambda calculus.

Pretty cool that you can do `:info (->)`

All functions in Haskell take one argument and return one result. (Partial
application)

```
Prelude> curry f a b = f (a, b)
Prelude> :t curry
curry :: ((t1, t) -> t2) -> t1 -> t -> t2
Prelude> :t fst
fst :: (a, b) -> a
Prelude> :t curry fst
curry fst :: t2 -> b -> t2
Prelude> curry fst 1 2
1

Prelude> uncurry f (a, b) = f a b
Prelude> :t uncurry
uncurry :: (t2 -> t1 -> t) -> (t2, t1) -> t
Prelude> :t (+)
(+) :: Num a => a -> a -> a
Prelude> (+) 1 2
3
Prelude> uncurry (+) (1, 2)
3
```

## Sectioning
```
Prelude> x = 5
Prelude> y = (2^)
Prelude> z = (^2)
Prelude> y x
32
Prelude> z x
25
```

```
Prelude> [1..10]
[1,2,3,4,5,6,7,8,9,10]
Prelude> elem 9 [1..10]
True
Prelude> 9 `elem` [1..10]
True
Prelude> hasTen = elem 10
Prelude> hasTen [1..15]
True
Prelude> hasTen [15..16]
False
Prelude> withinRange = (`elem` [1..10])
Prelude> withinRange 1
True
Prelude> withinRange 15
False
```

## Polymorphism

type signatures can be:
- concrete
- constrained polymorphic (ad-hoc polymorphism, typeclasses)
- parametric polymorphic

If a variable represents a set of possible values, then a type variable
represents a set of possible types. When there is no typeclass constraint, the
set of possible types a variable could represent is effectively unlimited.
Typeclass constraints limit the set of potential types (and, thus, potential
values) while also passing along the common functions that can be used with
those values.

## Exercises

## Summary

Polymorphism refers to type variables that can refer to multiple concrete types.
In Haskell it can be either parametric or ad-hoc (constrained). By having a
larger set of types we can intersect the commonalities of them all to produce a
smaller set of correct terms.

Type inference is the ability to infer the types without having to explicitly
write them down. In Haskell, the principal type is the most generic type that
still typecheckes. Principal typing holds for that type system if a type can be
found for a term in an environment for which all other types for that term are
instances of the principal type.

```
a
Num a => a
Int

-- Principal type will be a

(Ord a, Num a) => a
Integer

-- Principal type is (Ord a, Num a) => a
```

A type variable is a way to refer to unspecified type or set of types in Haskell
type signatures. Type signatures will usually be equal to themselves throughout
a type signature.

A typeclass is a means of expressing faculties or interfaces that multiple
datatypes may have in common. This enables us to write code exclusively in terms
of those commonalities without repeating yourself for each instance. In this way
we can sum values of type Int, Integer, Float, Double without having different
(+) or negate functions for each of them.

Parametricity is the property that holds in the presence of parametric
polymorphism. It states that the behaviour of a function will be uniform across
all concrete applications of the function. It is parametricity that tells us
that the function `id :: a -> a` will have the same behaviour for every type
without us needing to see how it was written.

Ad-hoc polymorphism applies to one or more typeclass constraints to what whould
have otherwise been a parametrically polymorphic type variable. The purpose of
ad-hoc polymorphism is to allow functions to have different behaviour for each
instance. For any given combination of typeclass and type, for example `Ord` and
`Bool`, there must only exist one unique instance in scope.

A module is the unit of organization used to collect together declaration of
values, functions, data types, typeclasses and typeclass instances. Any time you
use `import` in Haskell, you are importing declarations from a module.
