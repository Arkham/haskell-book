module Mood where

data Mood = Woot | Blah deriving Show

changeMood :: Mood -> Mood
changeMood Woot = Blah
changeMood Blah = Woot
