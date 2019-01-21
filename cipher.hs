module Cipher where

import Data.Char

-- Prelude Data.Char> :t chr
-- chr :: Int -> Char
-- Prelude Data.Char> :t ord
-- ord :: Char -> Int

caesar :: Int -> String -> String
caesar _ "" = ""
caesar steps (x:xs) = go x : caesar steps xs
  where go char
          | char `elem` ['a'..'z'] = caesarChar steps ['a'..'z'] char
          | char `elem` ['A'..'Z'] = caesarChar steps ['A'..'Z'] char
          | otherwise = char

uncaesar :: Int -> String -> String
uncaesar steps = caesar (-steps)

caesarChar :: Int -> String -> Char -> Char
caesarChar steps alphabet char =
  let
    start = ord (head alphabet)
    len = length alphabet
    shifted = ord char - start + steps
   in
  chr $ mod shifted len + start
