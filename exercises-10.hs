module Exercises10 where

-- stop-vowel-stop words
svs :: String -> String -> [String]
svs stops vowels =
  filter startsWithP all
  where all = [ [s1, v, s2] | s1 <- stops, v <- vowels, s2 <- stops ]
        startsWithP ('p':_) = True
        startsWithP _ = False

-- noun-verb-noun sentences
nvn :: [String] -> [String] -> [String]
nvn nouns verbs =
  [ n1 ++ " " ++ v ++ " " ++ n2 | n1 <- nouns, v <- verbs, n2 <- nouns ]
