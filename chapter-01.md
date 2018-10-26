# Lambda Calculus 🔬 

> A formal system for expressing computation in terms of **function** abstraction and application using **variable** binding and substitution

Invented in the 1930s by Alonzo Church. He was pals with Turing. Unsurprisingly, any problem which can be solved with a Turing machine can be solved with lambda calculus. Check out the aptly named [Church-Turing thesis](https://en.wikipedia.org/wiki/Church%E2%80%93Turing_thesis)!

## What is a function


    f(1) = A
    f(2) = B
    f(3) = C

The function `f` defines a set of relationships between the input set and the output set.
The input set is `{1,2,3}` and is also known as the *domain.*
The output set is `{A, B, C}` and is also known as the *codomain*.

## The golden rule of a function
> A function may never, ever, ever map to a different output when given the same input

So the following function is not kosher


    f(1) = A
    f(1) = B
    f(2) = C

The opposite is fine though


    f(1) = A
    f(2) = A
    f(3) = A

An example of function which maps multiple inputs to the same outputs is `modBy 2` .

## How do I write one?

Here is one is all its glory  `λ x . x`
Let’s see it in more detail 🙂 
 

    λ x . x
    ^─┬─^
      └────── the head of the lambda.
    
    λ x . x
      ^────── the single parameter of the
              function. This binds any
              variables with the same name
              in the body of the function.
    
    λ x . x
          ^── the body, the expression the lambda
              returns when applied. This is a
              bound variable.


## Alpha equivalence

Sounds complex, but it just means that all these expressions are equivalent


    λ x . x
    λ y . y
    λ z . z

It is also know as *alpha conversion*


## Beta reduction

Sounds complex, but it means to apply a function by


1. Substituting the input expression for all bound variables in the body
2. Removing the head of the expression

Let’s see some examples!


    (λ x . x) 2
    2


    (λ x . x) (λ y . y)
    λ y . y


    (λ x . x) (λ y . y) z
    (λ y . y) z
    z


## Free variables

Sometimes we have variables in the body of a function which are *unbound.*


    (λ x . xy) z
    zy

Alpha equivalence does not apply to free variables. The following two are NOT the same


    λ x . xz
    λ x . xy

But the following two are the same!


    λ x . xz
    λ y . yz


## Multiple arguments

Each lambda can only bind one parameter and accept one argument. To create functions that require multiple arguments we can apply it once, eliminate the first head and then apply the next. This was discovered by Haskell 🍛 and it known as *currying.*

This means that the following function


    λ xy . xy

can be written as


    λ x . (λ y . xy)

which can be applied as such


    (λ x . (λ y . xy)) 1 2
    (λ y . 1 y) 2
    1 2

But let’s be fancier and apply weirder arguments to our function


    (λ x . (λ y . xy)) (λ z . a) 2
    (λ y . (λ z . a) y) 2
    (λ z . a) 2
    a


## Normal form (or beta normal form)

When you can’t reduce the expression no more.

We don’t say


    (10 + 2) * 100 / 2

But just

`600`


## Combinators

A lambda term with no free variables. They’re cool because you know they only operate on their arguments.


## Divergence

It describes a reduction process that never ends. Here’s an example


    (λ x . xx)(λ x . xx)
    (λ x . xx)(λ x . xx)
    (λ x . xx)(λ x . xx)
    ...


## Summary
- Functional programming is based on expressions that include **variables** or **constants**, expressions combined with other expressions, and functions.
- Functions have a **head** and a **body**. Functions can be applied to arguments, reduced or evaluated to a result.
- Variables may be bound in the function declaration and every time a bound variable shows up in the function it assumes the same value.
- All functions take **one** argument and return **one** result.
- Functions are mappings of a set of **inputs** to a set of **outputs**. Given the same input, they always return the same output.

