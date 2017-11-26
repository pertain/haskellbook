-- Testing properties with QuickCheck -- end of chapter exercises (ch 14)

import Test.QuickCheck
import Data.List (sort)


-- 1)
-- Given this function...
half :: Fractional a => a -> a
--half x = x / 2
half = (/ 2)

-- This property should hold
halfIdentity :: Double -> Double
halfIdentity = (* 2) . half

prop_doubleThenHalve :: Double -> Bool
prop_doubleThenHalve x = x == halfIdentity x


-- 2)
-- For any list you apply sort to,
-- this property should hold
listOrdered :: Ord a => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
        where
            go _ status@(_, False) = status
            go y (Nothing, t) = (Just y, t)
            go y (Just x, _) = (Just y, x >= y)

prop_sortedIsSorted :: [Int] -> Bool
prop_sortedIsSorted xs = listOrdered (sort xs) == True


-- 3) These properties will not hold for floating point numbers
-- Test Associative and Commutative properties of addition
plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

prop_associativeAddition :: Int -> Int -> Int -> Bool
prop_associativeAddition x y z = plusAssociative x y z == True

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

prop_commutativeAddition :: Int -> Int -> Bool
prop_commutativeAddition x y = plusCommutative x y == True

-- 4) Now do the same for multiplication
multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

prop_associativeMultiplication :: Int -> Int -> Int -> Bool
prop_associativeMultiplication x y z = multAssociative x y z == True

multCommutative :: (Eq a, Num a) => a -> a -> Bool
multCommutative x y = x * y == y * x

prop_commutativeMultiplication :: Int -> Int -> Bool
prop_commutativeMultiplication x y = multCommutative x y == True

-- 5) There are some laws involving the relationship of
--    quot and rem and div and mod.
--    Write QuickCheck tests to prove them
quotRemLaw :: Integral a => a -> a -> Bool
quotRemLaw x y = (quot x y) * y + (rem x y) == x

divModLaw :: Integral a => a -> a -> Bool
divModLaw x y = (div x y) * y + (mod x y) == x

-- generate numerator and denominator values
-- but exclude denominators of 0
intNumerDenomGen :: Gen (Int, Int)
intNumerDenomGen = do
    a <- arbitrary
    b <- arbitrary `suchThat` (/= 0)
    return (a, b)

prop_noZeroDenoms :: Property
prop_noZeroDenoms =
    forAll intNumerDenomGen
    (\(_, y) -> y /= 0)

prop_quotRemLaw :: Property
prop_quotRemLaw =
    forAll intNumerDenomGen
    (\(x, y) -> quotRemLaw x y == True)

prop_divModLaw :: Property
prop_divModLaw =
    forAll intNumerDenomGen
    (\(x, y) -> divModLaw x y == True)


runQc :: IO ()
runQc = do
    quickCheck prop_doubleThenHalve
    quickCheck prop_sortedIsSorted
    quickCheck prop_associativeAddition
    quickCheck prop_commutativeAddition
    quickCheck prop_associativeMultiplication
    quickCheck prop_commutativeMultiplication
    quickCheck prop_noZeroDenoms
    quickCheck prop_quotRemLaw
    quickCheck prop_divModLaw
