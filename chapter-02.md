# Chapter 2

## GHCi commands

- :quit
- :info   (get info about a function)
- :module (to get out from module)
- :reload (reload the same file)

```
$ cat test.hs
sayHello :: String -> IO ()
sayHello x =
  putStrLn ("Hello, " ++ x ++ "!")
```

```
Prelude> :load test.hs
[1 of 1] Compiling Main             ( test.hs, interpreted )
Ok, modules loaded: Main.
*Main> sayHello "Haskell"
Hello, Haskell!
*Main>
```

## Expressions

Everything in Haskell is an expression or declaration.
- Expressions can be values, combination of values or functions applied to
  values. They evaluate to a result in a predictable and transparent manner.
- Declarations are top level bindings which allows us to name expressions.

Examples of expressions:
- 1
- 1 + 1
- "Icarus"

Expressions are in normal form when there are no more evaluation steps that can
be taken (they've reached an irreducible form). Reducible expressions are called
redexes instead. We can refer to the process as evaluating, reducing,
'normalizing' or 'executing' an expression.

## Functions

Every function only takes one argument and returns one result.
If a functions looks like it's taking more than one argument, it's actually just
applying a series of nested functions, each to one argument. This is called
_currying_.

In practice, the terms argument and parameter are often used interchangeably,
but there is a difference. Argument properly refers to the values that are
passed to the function’s parameters when the function is applied, not to the
variables that represent them in the function definition (or those in the type
signature).

## Evaluation

Haskell uses a lazy evaluation strategy, which defers evaluation of terms until
they're forced by other terms referring to them. In technical terms, it only
evaluates to weak head normal form (WHNF). So for example, this expression

```
(\f -> (1, 2 + f)) 2
```

reduces to the following in WHNF:

```
(1, 2 + 2)
```

## Infix operators

`ìd` the identity function

Some examples of infix operators
- 1 + 4
- 5 * 30
- 10 / 4

You can use normal operators in an infix way
- 10 `div` 4

Or infix operators in a prefix way
- (+) 10 4

Ghci will also tell you about the associativity and the precedence of an infix
operator:

```
Prelude> :info (+)
class Num a where
  (+) :: a -> a -> a
  ...
        -- Defined in ‘GHC.Num’
infixl 6 +

Prelude> :info (*)
class Num a where
  ...
  (*) :: a -> a -> a
  ...
        -- Defined in ‘GHC.Num’
infixl 7 *

Prelude> :info (^)
(^) :: (Num a, Integral b) => a -> b -> a       -- Defined in ‘GHC.Real’
infixr 8 ^
```

```
Prelude> 8 ^ 3 ^ 2
134217728
Prelude> (8 ^ 3) ^ 2
262144
Prelude> 8 ^ (3 ^ 2)
134217728
```

## Declaring values

- Order of declaration in the source doesn't matter because the file is loaded all
at once.
- Whitespace is important!
- All declarations must start on the same column

## Arithmetic functions

- `+`
- `-`
- `*`
- `/`
- `div`
- `mod`
- `quot`
- `rem`

### Difference mod and rem

Arithmetic modulo 12 means that 12 is equivalent to 0. So 11 + 4 in that
arithmetic is equal to 3.

Most times mod will give you the same result of rem

```
Prelude> mod 15 12
3
Prelude> rem 15 12
3
Prelude> mod 21 12
9
Prelude> rem 21 12
9
Prelude> mod 3 12
3
Prelude> rem 3 12
3
```

Let’s say we need to write a function that will determine what day of the week
it was or will be a certain number of days before or after this one.

Sunday will be 0. If today is Monday, which day will it be 23 days from now?

```
Prelude> mod (1 + 23) 7
3
```

And 5 days from Saturday will be Thursday:

```
Prelude> mod (6 + 5) 7
4
```

rem seems to do the same thing

```
Prelude> rem (1 + 23) 7
3
```

However if we want to subtract some days we will see a difference!
If today is Wednesday and we want to know what day it was 12 days ago

```
Prelude> mod (3 - 12) 7
5
Prelude> rem (3 - 12) 7
-2
```

The result of mod will have the same sign of the divisor, while the result of
rem will have the same sign of the dividend

### Negative numbers

Negative numbers need to be treated with care. This will break

```
Prelude> 1000 + -9

<interactive>:]18:1: error:
    Precedence parsing error
        cannot mix ‘+’ [infixl 6] and prefix `-' [infixl 6] in the same infix expression
```

But this works

```
Prelude> 1000 + (-9)
991
```

Which is the same as

```
Prelude> 1000 + (negate 9)
991
```

## $

What does it do??

```
Prelude> :info ($)
($) ::
  forall (r :: GHC.Types.RuntimeRep) a (b :: TYPE r).
  (a -> b) -> a -> b
        -- Defined in ‘GHC.Base’
infixr 0 $
```

And this is the definition

```
f $ a = f a
```

But since it's a right infix operator with the lowest precedence you can use it
to remove some pair of parentheses:

```
Prelude> (2^) (2 + 2)
16

Prelude> (2^) $ 2 + 2
16
```

Or a longer example

```
Prelude> (2^) $ (*30) $ 2 + 2
1329227995784915872903807060280344576

Prelude> (2^) $ (*30) $ 4
1329227995784915872903807060280344576

Prelude> (2^) $ 120
1329227995784915872903807060280344576

Prelude> 2 ^ 120
1329227995784915872903807060280344576
```

Writing something like `(+1)` is called sectioning.
With commutative functions writing `(+1)` or `(1+)` is the same.

```
Prelude> (1+) 3
4
Prelude> (+11) 3
14
```

But with non-commutative functions is not :)

```
Prelude> (/2) 3
1.5
Prelude> (2/) 3
0.6666666666666666
```

Unfortunately you can't write

```
(-1) 2
```

But you have to write

```
(subtract 1) 2
```

## Let and where

```
module FunctionWithWhere where

printInc n = print plusTwo
  where plusTwo = n + 2

printInc2 n = let plusTwo = n + 2
              in print plusTwo
```

```
Prelude> :l FunctionWithWhere.hs
[1 of 1] Compiling FunctionWithWhere ( FunctionWithWhere.hs, interpreted )
Ok, modules loaded: FunctionWithWhere.
*FunctionWithWhere> print
print     printInc
*FunctionWithWhere> printInc 3
5
*FunctionWithWhere> printInc2 3
5
```

