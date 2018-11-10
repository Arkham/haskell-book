# Chapter 3

## Types

You can use `:type` to check the type of a value, expression or function.

```
Prelude> :type 'a'
'a' :: Char
```

```
Prelude> :type "foobar"
"foobar" :: [Char]
```

String is a type alias for `[Char]`

We can print strings in various ways

```
Prelude> print "hello world"
"hello world"
Prelude> putStrLn "hello world"
hello world
Prelude> putStr "hello world"
```

```
module Print1 where

main :: IO ()
main = putStrLn "hello world!"
```

This function has a particular type: `IO ()`. It is used when we need to perform
effects beyond evaluating a function or an expression.

```
module Print2 where

main :: IO ()
main = do
  putStrLn "Count to four for me:"
  putStr   "one, two"
  putStr   ", three, and"
  putStrLn " four!"
```

```
Prelude> :l print2.hs
[1 of 1] Compiling Print2           ( print2.hs, interpreted )
Ok, modules loaded: Print2.
*Print2> main
Count to four for me:
one, two, three, and four!
*Print2>
```

## Concatenating strings

We can use either `++` or `concat`

```
*Print3> :t (++)
(++) :: [a] -> [a] -> [a]
*Print3> :t concat
concat :: Foldable t => t [a] -> [a]
```

Everything after the :: is about our types, not our values. The ‘a’ inside the
list type constructor [] is a type variable.

1. Take an argument of type [a]. This type is a list of elements of some type 𝑎.
   This function does not know what type 𝑎 is. It doesn’t need to know. In the
   context of the program, the type of 𝑎 will be known and made concrete at some
   point.

2. Take another argument of type [a], a list of elements whose type we don’t
   know. Because the variables are the same, they must be the same type
   throughout (a == a).

3. Return a result of type [a].

## More list functions

- cons
- head
- tail
- take n
- drop n
- (++)
- (!!)

Some of these functions are unsafe!

```
*Print3> "foobar" !! 10
*** Exception: Prelude.!!: index too large

*Print3> head ""
*** Exception: Prelude.head: empty list
```
