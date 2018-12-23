module DerivingInstances where

data Trivial = Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

data DayOfWeek =
  Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving Show

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

data Date =
  Date DayOfWeek Int
  deriving Show

instance Eq Date where
  (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') =
    weekday == weekday' && dayOfMonth == dayOfMonth'

f :: Int -> Bool
f 2 = True
f _ = False

data Identity a =
  Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn v) (TisAn v') = v == v'

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two v w) (Two v' w') =
    (v == v') && (w == w')

data StringOrInt =
  TisAnInt Int
    | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt v) (TisAnInt v') = v == v'
  (==) (TisAString v) (TisAString v') = v == v'
  (==) _ _ = False

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair v w) (Pair v' w') =
    (v == v') && (w == w')

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple v w) (Tuple v' w') =
    (v == v') && (w == w')

data Which a =
  ThisOne a
    | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne v) (ThisOne v') = v == v'
  (==) (ThisOne v) (ThatOne v') = v == v'
  (==) (ThatOne v) (ThisOne v') = v == v'
  (==) (ThatOne v) (ThatOne v') = v == v'

data EitherOr a b =
  Hello a
    | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello v) (Hello v') = v == v'
  (==) (Goodbye v) (Goodbye v') = v == v'
  (==) _ _ = False
