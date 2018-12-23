module Mood where

data Mood = Woot | Blah

changeMood :: Mood -> Mood
changeMood Woot = Blah
changeMood Blah = Woot

instance Show Mood where
  show Woot = "Woot"
  show Blah = "Blah"
