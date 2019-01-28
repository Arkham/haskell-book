# Folding lists

## Folds

Folds as a general concept are called catamorphisms. Cata means down or against,
as in catacombs. Morphism is about shapes. They are a mean of deconstructing
data.

If the spine of a list if the structure of a list, a fold is what can reduce
that structure.

```
Prelude> :t foldr
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
```

`foldr` associates to the right, while `foldl` associates to the left.
Here's a nice trick to visualize it:

```
Prelude> xs = map show [1..5]

Prelude> foldr (\x y -> concat ["(", x, "+", y, ")"]) "0" xs
"(1+(2+(3+(4+(5+0)))))"

Prelude> foldl (\x y -> concat ["(", x, "+", y, ")"]) "0" xs
"(((((0+1)+2)+3)+4)+5)"
```

The interesting thing in Haskell is that both `foldr` and `foldl` traverse the
list in the same direction to evaluate the values. The difference between right
folds and left folds is in the parenthesization of the folding function.

Another cool thing about foldr is that it can be written like this:

```
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)
```

The rest of the fold is described by `foldr f z xs`. So not only we can avoid
evaluating some of the values in the list, but we could also avoid evaluating
the spine! So we could use foldr with lists that are potentially infinite.

```
Prelude> foldr (+) 0 [1..5]
15
-- Note that this will evaluate all the values and the spine

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x b -> f x || b) False x
-- this will stop the first time we hit True

Prelude> myAny even [1..]
True

Prelude> myAny even (repeat 1)
-- Bottom
```

An interesting fact about folding is that bottoms both in the values or in the
spine will only be considered when we try to evaluate them

```
Prelude> foldr (+) 0 [1,2,3,4,undefined]
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:16:22 in interactive:Ghci8

Prelude> foldr (+) 0 $ take 4 [1,2,3,4,undefined]
10

Prelude> foldr (+) 0 ([1,2,3,4]++undefined)
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:20:25 in interactive:Ghci10

Prelude> foldr (+) 0 $ take 4 ([1,2,3,4]++undefined)
10
```

`take` is nonstrict and returns only as much as you ask for. Consider this case

```
Prelude> length $ take 2 $ take 4 ([1, 2] ++ undefined)
2
```

It doesn't matter that `take 4` would have hit the bottom, because it was
followed by a `take 2` :)

Let's write a simple foldr expression that never evaluates the cells:

```
Prelude> foldr (\_ _ -> 9001) 0 [1..5]
9001
Prelude> foldr (\_ _ -> 9001) 0 [1,2,3,undefined]
9001
Prelude> foldr (\_ _ -> 9001) 0 [undefined, undefined]
9001
Prelude> foldr (\_ _ -> 9001) 0 ([1,2,3] ++ undefined)
9001
Prelude> foldr (\_ _ -> 9001) 0 (undefined ++ [1,2,3])
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:30:25 in interactive:Ghci17
```

As we can see, everything is fine up until the first cons cell of the spine is
bottom. That first cons cell can contain bottom, but can't be bottom!

Let's see instead a function that evaluates the value inside the first cons
cell:

```
Prelude> :t const
const :: a -> b -> a
Prelude> foldr const 0 [1..5]
1
Prelude> foldr const 0 [1, undefined]
1
Prelude> foldr const 0 ([1] ++ undefined)
1
Prelude> foldr const 0 [undefined, 1]
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:35:16 in interactive:Ghci20
Prelude> foldr const 0 undefined
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:36:15 in interactive:Ghci20
```

## Fold Left

Left folds traverse the spine in the same direction as right folds, but their
folding process is left associative.

```
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs
```

We can use the same trick to show the associativity

```
Prelude> f x y = concat ["(", x, "+", y, ")"]
Prelude> foldl f "0" (map show [1..5])
"(((((0+1)+2)+3)+4)+5)"
```

We can also use _scans_ to see how folds evaluate. They return a list of all the
intermediate stages of the fold.

```
Prelude> foldr (+) 0 [1..5]
15

Prelude> foldl (+) 0 [1..5]
15

Prelude> scanr (+) 0 [1..5]
[15,14,12,9,5,0]

Prelude> scanl (+) 0 [1..5]
[0,1,3,6,10,15]
```

Lazy evaluation lets our functions dictate in what order things are evaluated.
See the following example:

```
foldr (^) 2 [1..3]
(1 ^ ( 2 ^ ( 3 ^ 2 )))
(1 ^ ( 2 ^ 9 ))
(1 ^ 512)
1

foldl (^) 2 [1..3]
(((2 ^ 1) ^ 2 ) ^ 3)
((2 ^ 2 ) ^ 3)
(4 ^ 3)
64
```

Pretty cool, eh? Another example

```
foldr (:) [] [1..3]
(1 : ( 2 : ( 3 : [] )))
(1 : ( 2 : [3] ))
(1 : [2, 3])
[1, 2, 3]

f = flip (:)
foldl (fcons) [] [1..3]
((([] `f` 1) `f` 2) `f` 3)
(([1] `f` 2) `f` 3)
([2, 1] `f` 3)
[3, 2, 1]
```

## Unconditional spine recursion

An important difference between `foldr` and `foldl` is that a left fold has the
successive steps of the fold as its first argument. The folding function
signature is `b -> a -> b`. Therefore the recursion on the spine in
unconditional. See the difference between:

```
Prelude> foldr const 0 ([1..5] ++ undefined)
1

-- the first time we run `const 1 (foldr const 0 ([2..5] ++ undefined))`
-- we notice that we don't need to run the rest of the
-- expression and we can return 1.


Prelude> foldr (flip const) 0 ([1..5] ++ undefined)
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:91:33 in interactive:Ghci58

-- in this case we run `const (foldr (flip const) 0 ([2..5] ++ undefined)) 1`
-- which makes us keep evaluating the spine until we hit undefined

foldl const 0 ([1..5] ++ undefined)
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:89:26 in interactive:Ghci58

-- this turns into `foldl const (const 0 1) ([2..5] ++ undefined)` and
-- therefore we can't control the recursion

Prelude> foldl (flip const) 0 ([1..5] ++ undefined)
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:90:33 in interactive:Ghci58

-- this turns into `foldl (flip const) (const 1 0) ([2..5] ++ undefined)`
-- and we still can't control the recursion
```

This means that `foldl` is generally inappropriate with lists that are infinite.
Not only that, since it forces the evaluation of the full spine, it is not
desirable even in cases when the list is long, since `foldl` will accumulate a
pile of unevaluated values as it traverses the spine. In most cases when you
need a left fold you should use `foldl'`. It is the same as `foldl` but it is
strict, which means it will force evaluation of the values as it traverses the
list.

## OMG

```
Prelude> "" == []
True
```

## Folding and evaluation

The right associativity of foldr means that the folding function evaluates from
the innermost cons cell to the outermost (the head). On the other had, foldl
recurses unconditionally to the end of the list through self-calls and then the
folding function evaluates from the outermost cons cell to the innermost.

```
Prelude> rcf = foldr (:) []
Prelude> xs = [1, 2, 3] ++ undefined
Prelude> take 3 $ rcf xs
[1,2,3]
Prelude> take 4 $ rcf xs
[1,2,3*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:114:19 in interactive:Ghci3

Prelude> lcf = foldl (flip (:)) []
Prelude> take 3 $ lcf xs
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:114:19 in interactive:Ghci3
```

## Summary

- in `foldr` the rest of the fold is an argument of the folding function you
  passed to `foldr`. The next invocation of `foldr` is conditional on the
  function having asked for more of the results. It associates to the right and
  works with infinite lists. It's a good default choice when you want to
  transform data structures, be they finite or infinite.
- `foldl` self-calls through the list, only beginning to produce values after
  reaching the end of the list. It associates to the left and it cannot be used
  with infinite lists. It is almost useless and should almost always be replaced
  with `foldl'`.

## Scans

```
Prelude> :t scanr
scanr :: (a -> b -> b) -> b -> [a] -> [b]
Prelude> :t scanl
scanl :: (b -> a -> b) -> b -> [a] -> [b]
```

## Fibonacci

```
Prelude> fibs = 1 : scanl (+) 1 fibs
Prelude> fibsN x = fibs !! x
Prelude> fibsN 140
131151201344081895336534324866
```

## Definitions

- A fold is a higher-order function which given a function and a recursive data
  structure, returns the built up value. Usually the start value of the
  accumulation is provided as well.
- A catamorphism is a generalization of folds to arbitrary datatypes.

```
Prelude> Data.Bool.bool 1 2 False
1
Prelude> Data.Bool.bool 1 2 True
2

Prelude> Data.Maybe.maybe 1 (* 2) Nothing
1
Prelude> Data.Maybe.maybe 1 (* 2) (Just 6)
12

Prelude> Data.Either.either (* 2) (* 4) (Left 1)
2
Prelude> Data.Either.either (* 2) (* 4) (Right 1)
4
```

- A tail call is the final result of a function.

- Tail recursion is a function whose tail calls are recursive invocations of
  itself. This is distinguished from functions that call other functions in
  their tail call, such as `f x y = f (x - 1) y`. For example `foldl`is tail
  recursive!
