module PoemLines where

tokenize :: Char -> String -> [String]
tokenize char str = reverse $ go str []
  where go str acc
          | str == "" = acc
          | otherwise =
            go (dropWhile (== char) . dropWhile (/= char) $ str)
              (takeWhile (/= char) str : acc)


myWords :: String -> [String]
myWords =
  tokenize ' '

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
            \ symmetry?"

sentences = firstSen ++ secondSen
  ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines =
  tokenize '\n'

shouldEqual :: [String]
shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?" ]

main :: IO ()
main =
  print $
    "Are the equal? "
    ++ show (myLines sentences == shouldEqual)
