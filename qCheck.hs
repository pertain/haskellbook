{-# LANGUAGE FlexibleInstances #-}

-- Testing properties with QuickCheck -- end of chapter exercises (ch 14)

import Test.QuickCheck
import Data.List (sort)


-- 01) Given this function...
half :: Fractional a => a -> a
half = (/ 2)

-- This property should hold
halfIdentity :: Double -> Double
halfIdentity = (* 2) . half

prop_doubleThenHalve :: Double -> Bool
prop_doubleThenHalve x = x == halfIdentity x


-- 02) For any list you apply sort to,
--     this property should hold
listOrdered :: Ord a => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
        where
            go _ status@(_, False) = status
            go y (Nothing, t) = (Just y, t)
            go y (Just x, _) = (Just y, x >= y)

prop_sortedIsSorted :: [Int] -> Bool
prop_sortedIsSorted xs = listOrdered (sort xs) == True


-- 03) These properties will not hold for floating point numbers
--     Test Associative and Commutative properties of addition
plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

prop_associativeAddition :: Int -> Int -> Int -> Bool
prop_associativeAddition x y z = plusAssociative x y z == True

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

prop_commutativeAddition :: Int -> Int -> Bool
prop_commutativeAddition x y = plusCommutative x y == True


-- 04) Now do the same for multiplication
multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

prop_associativeMultiplication :: Int -> Int -> Int -> Bool
prop_associativeMultiplication x y z = multAssociative x y z == True

multCommutative :: (Eq a, Num a) => a -> a -> Bool
multCommutative x y = x * y == y * x

prop_commutativeMultiplication :: Int -> Int -> Bool
prop_commutativeMultiplication x y = multCommutative x y == True


-- 05) There are some laws involving the relationship of
--     quot and rem and div and mod.
--     Write QuickCheck tests to prove them
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

-- 06) Is (^) associative? Is it commutative?
--     Use QuickCheck to see if the computer
--     can contradict such an assertion.
expAssociative :: Integral a => a -> a -> a -> Bool
expAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

prop_associativeExponentiation :: Int -> Int -> Int -> Bool
prop_associativeExponentiation x y z = expAssociative x y z == True

expCommutative :: Integral a => a -> a -> Bool
expCommutative x y = x ^ y == y ^ x

prop_commutativeExponentiation :: Int -> Int -> Bool
prop_commutativeExponentiation x y = expCommutative x y == True


-- 07) Test that reversing a list twice is the same as
--     the identity of the list.
revRevIsId :: Eq a => [a] -> Bool
revRevIsId xs = (reverse . reverse) xs == id xs

prop_revRevIsId :: [Char] -> Bool
prop_revRevIsId xs = revRevIsId xs == True


-- 08) Write a property for the definition of ($)
-- These Show instances are needed for CoArbitrary
instance Show (Int -> Char) where
    show _ = "Function: (Int -> Char)"

instance Show (Char -> [Char]) where
    show _ = "Function: (Char -> [Char])"

dollar :: Eq a => (t -> a) -> t -> Bool
dollar f a = (f $ a) == f a

-- Uses CoArbitrary to generate random functions
-- that fit the type signature (Int -> Char)
prop_dollar :: (Int -> Char) -> Int -> Bool
prop_dollar f a = dollar f a == True

dot :: Eq a1 => (b -> a1) -> (a2 -> b) -> a2 -> Bool
dot f g x = (f . g) x == f (g x)

-- Uses CoArbitrary to generate random functions
-- that fit type signatures (Int -> Char) and (Char -> [Char])
prop_dot :: (Char -> [Char]) -> (Int -> Char) -> Int -> Bool
prop_dot f g x = dot f g x == True


-- 09) See if these two functions are equal:
--      >> foldr (:) == (++)
--      >> foldr (++) [] == concat
prop_foldrAppend :: [Int] -> [Int] -> Bool
prop_foldrAppend xs ys = foldr (:) xs ys == xs ++ ys

prop_foldrConcat :: [String] -> Bool
prop_foldrConcat xs = foldr (++) [] xs == concat xs


-- 10) Determine if this function is correct
--      >> f n xs = length (take n xs) == n
prop_lengthOfTake :: Int -> String -> Bool
prop_lengthOfTake n xs = length (take n xs) == n


-- 11) Compose read and show, then test it.
--      >> f x = (read (show x)) == x
prop_readShow :: Int -> Bool
prop_readShow x = (read (show x)) == x
    

runQc :: IO ()
runQc = do
    -- 01)
    quickCheck prop_doubleThenHalve

    -- 02)
    quickCheck prop_sortedIsSorted

    -- 03)
    quickCheck prop_associativeAddition
    quickCheck prop_commutativeAddition

    -- 04)
    quickCheck prop_associativeMultiplication
    quickCheck prop_commutativeMultiplication

    -- 05)
    quickCheck prop_noZeroDenoms
    quickCheck prop_quotRemLaw
    quickCheck prop_divModLaw

    -- 06) Exponentiation is neither associative
    --     nor commutative
    quickCheck prop_associativeExponentiation   -- Should fail
    quickCheck prop_commutativeExponentiation   -- Should fail

    -- 07)
    quickCheck prop_revRevIsId

    -- 08)
    quickCheck prop_dollar
    quickCheck prop_dot

    -- 09)
    quickCheck prop_foldrAppend   -- Should fail
    quickCheck prop_foldrConcat

    -- 10)
    quickCheck prop_lengthOfTake    -- Should fail

    -- 11)
    quickCheck prop_readShow
