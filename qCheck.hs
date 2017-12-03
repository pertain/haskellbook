{-# LANGUAGE ViewPatterns #-}

-- Testing properties with QuickCheck -- end of chapter exercises (ch 14)

import Test.QuickCheck
import Test.QuickCheck.Function
import Data.List (sort)


-- 01) Given this function...
half :: Fractional a => a -> a
half = (/ 2)

-- This property should hold
halfIdentity :: Fractional a => a -> a
halfIdentity = (* 2) . half

prop_doubleThenHalve :: (Fractional a, Eq a) => a -> Bool
prop_doubleThenHalve x = x == halfIdentity x

-- Test the property with Double
type DoubleHalveDouble = Double -> Bool


-- 02) For any list you apply sort to,
--     this property should hold
listOrdered :: Ord a => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
        where
            go _ status@(_, False) = status
            go y (Nothing, t) = (Just y, t)
            go y (Just x, _) = (Just y, x >= y)

prop_sortedIsSorted :: Ord a => [a] -> Bool
prop_sortedIsSorted = listOrdered . sort

-- Test the property with Int
type SortedIsSortedInt = [Int] -> Bool


-- 03) These properties will not hold for floating point numbers
--     Test Associative and Commutative properties of addition
prop_assocAdd :: (Eq a, Num a) => a -> a -> a -> Bool
prop_assocAdd x y z = x + (y + z) == (x + y) + z

-- Test the property with Int
type AssocAddInt = Int -> Int -> Int -> Bool

prop_commuteAdd :: (Eq a, Num a) => a -> a -> Bool
prop_commuteAdd x y = x + y == y + x

-- Test the property with Int
type CommuteAddInt = Int -> Int -> Bool


-- 04) Now do the same for multiplication
prop_assocMult :: (Eq a, Num a) => a -> a -> a -> Bool
prop_assocMult x y z = x * (y * z) == (x * y) * z

-- Test the property with Int
type AssocMultInt = Int -> Int -> Int -> Bool

prop_commuteMult :: (Eq a, Num a) => a -> a -> Bool
prop_commuteMult x y = x * y == y * x

-- Test the property with Int
type CommuteMultInt = Int -> Int -> Bool


-- 05) There are some laws involving the relationship of
--     quot and rem and div and mod.
--     Write QuickCheck tests to prove them
quotRemLaw :: Integral a => a -> a -> Bool
quotRemLaw x y = (quot x y) * y + (rem x y) == x

divModLaw :: Integral a => a -> a -> Bool
divModLaw x y = (div x y) * y + (mod x y) == x

-- generate Int values for numerator and denominator,
-- but exclude 0 in denominators
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
    (uncurry quotRemLaw)

prop_divModLaw :: Property
prop_divModLaw =
    forAll intNumerDenomGen
    (uncurry divModLaw)

-- 06) Is (^) associative? Is it commutative?
--     Use QuickCheck to see if the computer
--     can contradict such an assertion.
prop_assocExp :: Integral a => a -> a -> a -> Bool
prop_assocExp x y z = x ^ (y ^ z) == (x ^ y) ^ z

-- Test the property with Int
type AssocExpInt = Int -> Int -> Int -> Bool

prop_commuteExp :: Integral a => a -> a -> Bool
prop_commuteExp x y = x ^ y == y ^ x

-- Test the property with Int
type CommuteExpInt = Int -> Int -> Bool


-- 07) Test that reversing a list twice is the same as
--     the identity of the list.
prop_revRevIsId :: Eq a => [a] -> Bool
prop_revRevIsId xs = (reverse . reverse) xs == id xs

-- Test the property with Char
type RevRevIdChar = [Char] -> Bool


-- 08) Write a property for the definition of ($)
dollar :: Eq b => (a -> b) -> a -> Bool
dollar f x = (f $ x) == f x

-- Uses QuickCheck's Function module to generate
-- functions of type (Int -> Char)
prop_dollar :: Eq b => a -> Fun a b -> Bool
prop_dollar x (apply -> f) = dollar f x

-- Test the property with Int and (Int -> Char)
type DollarProp = Int
               -> Fun Int Char
               -> Bool

-- Write a property for the definition of (.)
dot :: Eq c => (b -> c) -> (a -> b) -> a -> Bool
dot f g x = (f . g) x == f (g x)

-- Uses QuickCheck's Function module to generate
-- functions of type (Char -> [Char]) and (Int -> Char)
prop_dot :: Eq c => a -> Fun b c -> Fun a b -> Bool
prop_dot x (apply -> f) (apply -> g) = dot f g x

-- Test the property with Int, (Char -> [Char]), and (Int -> Char)
type DotProp = Int
            -> Fun Char [Char]
            -> Fun Int Char
            -> Bool


-- 09) See if these two functions are equal:
--      >> foldr (:) == (++)
--      >> foldr (++) [] == concat
prop_foldrAppend :: Eq a => [a] -> [a] -> Bool
prop_foldrAppend xs ys = foldr (:) xs ys == xs ++ ys

-- Test the property with Int
type FoldrAppendInt = [Int] -> [Int] -> Bool

prop_foldrConcat :: (Traversable t, Eq a) => t [a] -> Bool
prop_foldrConcat xs = foldr (++) [] xs == concat xs

-- Test the property with String
type FoldrConcatString = [String] -> Bool


-- 10) Determine if this function is correct
--      >> f n xs = length (take n xs) == n
prop_lengthOfTake :: Int -> [a] -> Bool
prop_lengthOfTake n xs = length (take n xs) == n

-- Test the property with Char
type LengthOfTakeChar = Int -> [Char] -> Bool


-- 11) Compose read and show, then test it.
--      >> f x = (read (show x)) == x
prop_readShow :: (Eq a, Show a, Read a) => a -> Bool
prop_readShow x = (read (show x)) == x

type ReadShowInt = Int -> Bool
    

runQc :: IO ()
runQc = do
    -- 01)
    putStrLn "\nprop_doubleThenHalve (Should Pass)"
    quickCheck (prop_doubleThenHalve :: DoubleHalveDouble)

    -- 02)
    putStrLn "\nprop_sortedIsSorted (Should Pass)"
    quickCheck (prop_sortedIsSorted :: SortedIsSortedInt)

    -- 03)
    putStrLn "\nprop_assocAdd (Should Pass)"
    quickCheck (prop_assocAdd :: AssocAddInt)
    putStrLn "\nprop_commuteAdd (Should Pass)"
    quickCheck (prop_commuteAdd :: CommuteAddInt)

    -- 04)
    putStrLn "\nprop_assocMult (Should Pass)"
    quickCheck (prop_assocMult :: AssocMultInt)
    putStrLn "\nprop_commuteMult (Should Pass)"
    quickCheck (prop_commuteMult :: CommuteMultInt)

    -- 05)
    putStrLn "\nprop_noZeroDenoms (Should Pass)"
    quickCheck prop_noZeroDenoms
    putStrLn "\nprop_quotRemLaw (Should Pass)"
    quickCheck prop_quotRemLaw
    putStrLn "\nprop_divModLaw (Should Pass)"
    quickCheck prop_divModLaw

    -- 06) Exponentiation is neither associative nor commutative
    putStrLn "\nprop_assocExp (Should Fail)"
    quickCheck (prop_assocExp :: AssocExpInt)
    putStrLn "\nprop_commuteExp (Should Fail)"
    quickCheck (prop_commuteExp :: CommuteExpInt)

    -- 07)
    putStrLn "\nprop_revRevIsId (Should Pass)"
    quickCheck (prop_revRevIsId :: RevRevIdChar)

    -- 08)
    putStrLn "\nprop_dollar (Should Pass)"
    quickCheck (prop_dollar :: DollarProp)
    putStrLn "\nprop_dot (Should Pass)"
    quickCheck (prop_dot :: DotProp)

    -- 09) foldr (:) is not synonymous with (++)
    putStrLn "\nprop_foldrAppend (Should Fail)"
    quickCheck (prop_foldrAppend :: FoldrAppendInt)
    putStrLn "\nprop_foldrConcat (Should Pass)"
    quickCheck (prop_foldrConcat :: FoldrConcatString)

    -- 10)
    putStrLn "\nprop_lengthOfTake (Should Fail)"
    quickCheck (prop_lengthOfTake :: LengthOfTakeChar)

    -- 11)
    putStrLn "\nprop_readShow (Should Pass)"
    quickCheck (prop_readShow :: ReadShowInt)
