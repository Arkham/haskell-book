module Where where

mult1 = x
  where x = 5

mult2 = x * x
  where x = 5

mult3 = x * y
  where x = 5
        y = 6

mult4 = x + 3
  where x = 3
        y = 10000

other1 = x * 3 + y
  where x = 3
        y = 1000

other2 = x * 5
  where y = 10
        x = 10 * 5 + y

other3 = z / x + y
  where x = 7
        y = negate x
        z = y * 10

all =
  [ mult1, mult2, mult3, mult4, other1, other2, other3 ]
