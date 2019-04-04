# Signaling adversity

## Maybe

```
data Maybe a = Nothing | Just a
```

```
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | name /= "" && age >= 0 =
    Just $ Person name age
  | otherwise = Nothing
```

```
Prelude> mkPerson "Ju" 10
Just (Person "Ju" 10)
Prelude> mkPerson "Ju" (-10)
Nothing
```

## Either

```
data Either a b = Left a | Right b
```

```
data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)
```

Case expressions and pattern matching will work without an Eq instance, but
guards using (==) will not.

```
mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age >= 0 =
    Right $ Person name age
  | name == "" = Left NameEmpty
  | otherwise = Left AgeTooLow
```

It is a convention that the Left of Either is used for whatever case is going to
cause the work to stop. (Mnemonic: right is right)

```
Prelude> mkPerson "" 10
Left NameEmpty
Prelude> mkPerson "Djali" (-1)
Left AgeTooLow
Prelude> mkPerson "" (-1)
Left NameEmpty
```

Note that in this way if both errors happen we only know about the first one.
But we can fix that!

```
type ValidatePerson a =
  Either [PersonInvalid] a

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
  True -> Right age
  False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
  True -> Right name
  False -> Left [NameEmpty]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age =
  mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) =
  Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) =
  Left (badName ++ badAge)
mkPerson' (Left badName) _ =
  Left badName
mkPerson' _ (Left badAge) =
  Left badAge
```

See it in all its glory

```
Prelude> mkPerson "" (-1)
Left [NameEmpty,AgeTooLow]
Prelude> mkPerson "Foobar" (-1)
Left [AgeTooLow]
Prelude> mkPerson "Foobar" 18
Right (Person "Foobar" 18)
```

## Higher kinded types

```
Prelude> data Example a = Blah | Woot a
Prelude> :k Example
Example :: * -> *
```

Let's see the kinds of Maybe and Either

```
Prelude> :k Maybe
Maybe :: * -> *
Prelude> :k Either
Either :: * -> * -> *
```

## Data constructors are functions

```
data Unary a = Unary a deriving Show

Prelude> :t Unary
Unary :: a -> Unary a
Prelude> :t Unary 10
Unary 10 :: Num a => Unary a
Prelude> :t Unary "blah"
Unary "blah" :: Unary [Char]
```

For example, even `Just` is a function

```
fmap Just [1,2,3]
[Just 1,Just 2,Just 3]
```

## String processing

1. Writw a function which takes a string, breaks it into words and
   replaces each instance of "the" with "a".

   ```
   notThe :: String -> Maybe String
   notThe "the" = Nothing
   notThe other = Just other

   replaceThe :: String -> String
   replaceThe str =
     Data.List.intercalate " " $
       map (Data.Maybe.maybe "a" id . notThe) (words str) 
   ```

## Definitions

1. A higher kinded type is any type whose kind has a function arrow in it and
   which can be described as a type constructor rather than a type constant.
