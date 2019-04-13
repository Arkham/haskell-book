module ValidateCow where

import Control.Applicative

data Cow = Cow
  { name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' =
  liftA3 Cow (noEmpty name') (noNegative age') (noNegative weight')
--
-- *ValidateCow> :t Cow
-- Cow :: String -> Int -> Int -> Cow
-- *ValidateCow> cow1 = Cow <$> noEmpty "bees"
-- *ValidateCow> :t cow1
-- cow1 :: Maybe (Int -> Int -> Cow)
-- *ValidateCow> cow2 = cow1 <*> noNegative 1
-- *ValidateCow> :t cow2
-- cow2 :: Maybe (Int -> Cow)
-- *ValidateCow> cow3 = cow2 <*> noNegative 2
-- *ValidateCow> :t cow3
-- cow3 :: Maybe Cow
-- *ValidateCow> :t liftA3 Cow
-- liftA3 Cow :: Applicative f => f String -> f Int -> f Int -> f Cow
