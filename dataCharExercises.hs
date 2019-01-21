module DataCharExercises where

import Data.Char

-- Prelude Data.Char> :t isUpper
-- isUpper :: Char -> Bool
-- Prelude Data.Char> :t toUpper
-- toUpper :: Char -> Char

filterUpper :: String -> String
filterUpper =
  filter isUpper

capitalizeFirst :: String -> String
capitalizeFirst "" = ""
capitalizeFirst (x:xs) = toUpper x : xs

capitalizeAll :: String -> String
capitalizeAll "" = ""
capitalizeAll (x:xs) = toUpper x : capitalizeAll xs

-- Prelude> :t head
-- head :: [a] -> a
-- Prelude> head []
-- *** Exception: Prelude.head: empty list

getFirstCapitalized :: String -> Char
getFirstCapitalized = toUpper . head
