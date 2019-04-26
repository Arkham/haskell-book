module Addition where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

multiplyBySum :: (Eq a, Num a) => a -> a -> a
multiplyBySum a b = multiplyBySum' a b 0
  where
    multiplyBySum' x y acc
      | x == 0 = acc
      | y == 0 = acc
      | otherwise = multiplyBySum' (x - 1) y (acc + y)

main :: IO ()
main =
  hspec $ do
    describe "Addition" $ do
      it "1 + 1 is greater than 1" $ (1 + 1) > (1 :: Int) `shouldBe` True
      it "2 + 2 is equal to 4" $ 2 + 2 `shouldBe` (4 :: Int)
    describe "dividedBy" $ do
      it "15 div by 3 is 5" $ dividedBy 15 3 `shouldBe` ((5, 0) :: (Int, Int))
      it "22 div by 5 is 4 with rem 2" $
        dividedBy 22 5 `shouldBe` ((4, 2) :: (Int, Int))
    describe "multiplyBySum" $ do
      it "5 multiplied by 9 is 45" $ multiplyBySum 5 9 `shouldBe` (45 :: Int)
      it "5 multiplied by 0 is 0" $ multiplyBySum 5 0 `shouldBe` (0 :: Int)
      it "0 multiplied by 5 is 0" $ multiplyBySum 0 5 `shouldBe` (0 :: Int)
    describe "QuickCheck" $
      it "x + 1 is always > 1" $ property $ \x -> x + 1 > (x :: Int)

-- generators
genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a' .. 'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

-- more Just then Nothing
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [(1, return Nothing), (3, return (Just a))]

-- use QuickCheck without hspec
prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 0 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
