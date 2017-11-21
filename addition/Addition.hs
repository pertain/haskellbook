-- Addition.hs

module Addition where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > 1 `shouldBe` True
        it "2 + 2 is equal to 4" $ do
            2 + 2 `shouldBe` 4
    describe "Division" $ do
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5, 0)
        it "22 divided by 5 is 4 remainder 2" $ do
            dividedBy 22 5 `shouldBe` (4, 2)
    describe "Multiplication" $ do
        it "4 multiplied by 4 is 16" $ do
            sumBy 4 4 `shouldBe` 16
        it "8 multiplied by 9 is 72" $ do
            sumBy 8 9 `shouldBe` 72
    describe "Properties" $ do
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int) -- using QuickCheck inside hspec

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where
        go n d count
            | n < d     = (count, n)
            | otherwise = go (n - d) d (count + 1)

sumBy :: (Eq a, Num a) => a -> a -> a
sumBy num1 num2 = go num1 num2 0
    where
        go x y total
            | y == 0    = total
            | otherwise = go x (y - 1) (total + x)


-- Playing with Gen (QuickCheck)

-- Only a single value is possible
trivialInt :: Gen Int
trivialInt = return 1

-- Each of 1, 2, and 3 have the same probability of being picked
oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

-- Now 2 has a higher probability of being picked
oneThroughThree' :: Gen Int
oneThroughThree' = elements [1, 2, 2, 2, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

--

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

-- equal probability
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
    a <- arbitrary
    elements [Nothing, Just a]

-- This is what QuickCheck does so
-- you get more Just values
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
    a <- arbitrary
    frequency [ (1, return Nothing)
              , (3, return (Just a))]


-- Use QuickCheck without hspec

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

prop_subtractionLessThan :: Int -> Bool
prop_subtractionLessThan x = x - 1 < x

-- This will fail (intentionally)
prop_additionLessThan :: Int -> Bool
prop_additionLessThan x = x - 1 > x

-- runQc is like main for QuickCheck
runQc :: IO ()
runQc = do
    quickCheck prop_additionGreater
    quickCheck prop_subtractionLessThan
    quickCheck prop_additionLessThan
