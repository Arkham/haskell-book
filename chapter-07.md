# More Functional Patterns

We will see that Haskell functions:

- are first-class entities
- can be values in expressions, lists and tuples
- can be passed as arguments to a function
- can be returned from a function as a result
- make use of syntactic patterns

Haskell is _lexically_ scoped. Lexical scoping means that resolving the value
for a named entity depends on the location in the code and the lexical context,
for example in `let` and `where` clauses.

```
Prelude> x = 5
Prelude> y = x + 5
Prelude> y
10
Prelude> y * 10
100
Prelude> z y = y * 10
Prelude> x
5
Prelude> y
10
Prelude> z 9
90
Prelude> z y
100
```

### Anonymous functions

You can write

```
triple :: Integer -> Integer
triple x = x * 3
```

as

```
(\x -> x * 3) :: Integer -> Integer
```

### Pattern matching

Patterns are matched against values, or data constructors, not types. The order
of pattern matches matters! For example the following code

```
isItTwo _ = False
isItTwo 2 = True
```

will always return `False`! GHC will also tell you something

```
<interactive>:20:1: warning: [-Woverlapping-patterns]
    Pattern match is redundant
    In an equation for ‘isItTwo’: isItTwo 2 = ...
```

### newtype

`newtype` is a special case of `data` declaration: it only permits only one
constructor and only one field.

### browse

```
:browse MatchingTuples
addEmUp2 :: Num a => (a, a) -> a
addEmUp2Alt :: Num a => (a, a) -> a
fst3 :: (a, b, c) -> a
third3 :: (a, b, c) -> c
```

### HOF

Higher order functions are functions that accept functions as arguments.

```
Prelude> :t flip
flip :: (a -> b -> c) -> b -> a -> c
```

Remember that `->` is right associative!

### Guards

```

Prelude> :{
Prelude| myAbs :: Integer -> Integer
Prelude| myAbs x
Prelude|   | x < 0 = (-x)
Prelude|   | otherwise = x
Prelude| :}
Prelude> myAbs 3
3
Prelude> myAbs (-3)
3
```

```
Prelude> otherwise == True
True
```

### Function composition

```
Prelude> :t (.)
(.) :: (b -> c) -> (a -> b) -> a -> c
```

```
negate . sum $ xs
```

You may be wondering why we need the $ operator. Ordinary function application
has a precedence of 10. The composition operator has a precedence of 9. If we
left white space as our function application, this would be evaluated like this:

```
negate . sum [1, 2, 3, 4, 5]
negate . 15
```

Using the composition operator is nice because it lets us easily compose more
functions this way. For example use could write something like
g```
Prelude> take 5 . filter odd . enumFrom $ 3
[3,5,7,9,11]
```

### Pointfree style

The point in pointfree refers to the arguments.

```
Prelude> f :: Int -> [Int] -> Int; f z xs = foldr (+) z xs
Prelude> f 0 [1..10]
55

Prelude> f :: Int -> [Int] -> Int; f = foldr (+)
Prelude> f 10 [1..5]
25
```

### Demonstrating composition

```
Prelude> :t putStr
putStr :: String -> IO ()
Prelude> :t putStrLn
putStrLn :: String -> IO ()
Prelude> :t print
print :: Show a => a -> IO ()
```

### Read type class

```
read :: Read a => String -> a
show :: Show a => a -> String
```
### Definitions

- A parameter variable is _bound_ to an argument value, meaning the value is
  passed into the parameter as input and each occurrence of that named parameter
  will have the same value.
- An _anonymous function_ is a function which is not bound to an identifier. So
  a bound function would be `id x = x` and its anonymous counterpart would be
  `\x -> x`
- _Currying_ is the process of transforming a function that takes multiple
  arguments into a series of functions which each take one argument and return
  one result. In Haskell, all functions are curried by default!
- _Pattern matching_ is a syntactic way of deconstructing product and sum types
  to get at their inhabitants. It allows to destructure and expose the contents
  of a product type, binding one or more values. With regards to sum types, it
  allows to discriminate which inhabitant of the sum type you want to handle in
  that match.
- _Bottom_ is a non-value used to denote that a program cannot return a value or
  a result.
- _Higher order functions_ are functions which themselves take functions as
  arguments or return functions as results.
- _Composition_ is a the application of a function to the result of having
  applied another function. _Pointfree_ is programming without mentioning
  arguments by name.
