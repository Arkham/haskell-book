module OrdInstances where

data DayOfWeek =
  Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Eq, Show)

-- what if we wanted friday to be the best day?
instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _   = GT
  compare _   Fri = LT
  compare _   _   = EQ

a =
  max (length [ 1, 2, 3]) (length [ 8, 9, 10, 11, 12 ])

b =
  compare (3 * 4) (3 * 5)

c =
  -- compare "Julie" True -- crash
  compare "Julie" "John"

d =
  (5 + 3) > (3 + 6)
