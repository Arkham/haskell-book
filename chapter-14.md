# Testing

See `addition` folder

## Conventional Testing

Use `hspec`

## Property Testing

Use `QuickCheck`

## Arbitrary instances

```
Prelude> :t arbitrary
arbitrary :: Arbitrary a => Gen a

Prelude> :t sample
sample :: Show a => Gen a -> IO ()

Prelude> :t sample'
sample' :: Gen a -> IO [a]
```

We use the `Arbitrary` type class to provide a generator for `sample`.

```
Prelude> sample (arbitrary :: Gen Int)
0
-2
-1
-2
7
10
9
-5
11
-6
-2

Prelude> sample (arbitrary :: Gen Double)
0.0
1.9483601499116319
2.4296151687394283
-1.6014374732406225
-7.39232071553302
7.549355028665763
1.1323741761146933
3.961424169285916
6.486130802963825
11.192142987095155
15.853678738154867
```

We can also write our own data for generating `Gen` values.

```
trivialInt :: Gen Int
trivialInt = return 1

Prelude> sample' (trivialInt)
[1,1,1,1,1,1,1,1,1,1,1]

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

Prelude> sample' oneThroughThree
[3,2,2,1,3,2,3,1,3,3,1]

oneThroughThree' :: Gen Int
oneThroughThree' = elements [1, 2, 2, 2, 2, 3]

Prelude> sample' oneThroughThree'
[3,2,1,3,2,2,2,2,2,1,3]
```

Check out more generators inside `addition/Addition.hs`

Remember that if you don't specify the type you will get a bunch of `()`

```
Prelude> sample' genTuple
[((),()),((),()),((),()),((),()),((),()),((),()),((),()),((),()),((),()),((),()),((),())]

Prelude> sample' (genTuple :: Gen (Int, Float))
[(0,0.0),(0,-0.79074216),(0,0.71437985),(-4,-0.4651646),(6,-3.170394),(3,-6.912019),(-8,-6.828774),(-1,-10.813282),(-4,2.729955),(1,1.4738456),(-20,-13.478984)]

Prelude> sample' (genTuple :: Gen (Int, String))
[(0,""),(-1,""),(4,"2t'"),(6,"OZ"),(-4,"\EOT"),(6,""),(4,"9\659848\585122.\8781y\DC1'"),(13,"RYi\GS\127904"),(12,"i\a\25071R'\USO&\ETX^j\465630\1000685Dir"),(14,"\856895\&4\v\b\638565yJ\359003"),(14,"\578259H\11951")]
```

You can always use `:info Arbitrary` to see what instances are available.

## Using QuickCheck without hspec

```
*Addition> runQc
+++ OK, passed 100 tests.

*Addition> runQc
*** Failed! Falsifiable (after 1 test):
0
```
