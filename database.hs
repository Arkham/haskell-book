module Database where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
    (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
    (fromGregorian 1921 5 1)
    (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate =
  foldr
    (\item acc ->
      case item of
        DbDate time ->
          time : acc

        _ ->
          acc
    )
    []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber =
  foldr
    (\item acc ->
      case item of
        DbNumber num ->
          num : acc

        _ ->
          acc
    )
    []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent =
  maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb =
  sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb list =
  let numbers = filterDbNumber list
      count = length numbers
      total = sum numbers
   in
    (fromIntegral total) / (fromIntegral count)
