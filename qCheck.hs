-- Testing properties with QuickCheck -- end of chapter exercises (ch 14)

import Test.QuickCheck
import Test.QuickCheck.Function
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
dollar :: Eq b => (a -> b) -> a -> Bool
dollar f a = (f $ a) == f a

-- Uses QuickCheck's Function module to generate
-- functions of type (Int -> Char)
prop_dollar :: Int -> (Fun Int Char) -> Bool
prop_dollar n (Fun _ f) = dollar f n == True

dot :: Eq a1 => (b -> a1) -> (a2 -> b) -> a2 -> Bool
dot f g x = (f . g) x == f (g x)

-- Uses QuickCheck's Function module to generate
-- functions of type (Char -> [Char]) and (Int -> Char)
prop_dot :: Int -> (Fun Char [Char]) -> (Fun Int Char) -> Bool
prop_dot n (Fun _ f) (Fun _ g) = dot f g n == True


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
    putStrLn "\nprop_doubleThenHalve (Should Pass)"
    quickCheck prop_doubleThenHalve

    -- 02)
    putStrLn "\nprop_sortedIsSorted (Should Pass)"
    quickCheck prop_sortedIsSorted

    -- 03)
    putStrLn "\nprop_associativeAddition (Should Pass)"
    quickCheck prop_associativeAddition
    putStrLn "\nprop_commutativeAddition (Should Pass)"
    quickCheck prop_commutativeAddition

    -- 04)
    putStrLn "\nprop_associativeMultiplication (Should Pass)"
    quickCheck prop_associativeMultiplication
    putStrLn "\nprop_commutativeMultiplication (Should Pass)"
    quickCheck prop_commutativeMultiplication

    -- 05)
    putStrLn "\nprop_noZeroDenoms (Should Pass)"
    quickCheck prop_noZeroDenoms
    putStrLn "\nprop_quotRemLaw (Should Pass)"
    quickCheck prop_quotRemLaw
    putStrLn "\nprop_divModLaw (Should Pass)"
    quickCheck prop_divModLaw

    -- 06) Exponentiation is neither associative nor commutative
    putStrLn "\nprop_associativeExponentiation (Should Fail)"
    quickCheck prop_associativeExponentiation
    putStrLn "\nprop_commutativeExponentiation (Should Fail)"
    quickCheck prop_commutativeExponentiation

    -- 07)
    putStrLn "\nprop_revRevIsId (Should Pass)"
    quickCheck prop_revRevIsId

    -- 08)
    putStrLn "\nprop_dollar (Should Pass)"
    quickCheck prop_dollar
    putStrLn "\nprop_dot (Should Pass)"
    quickCheck prop_dot

    -- 09) foldr (:) is not synonymous with (++)
    putStrLn "\nprop_foldrAppend (Should Fail)"
    quickCheck prop_foldrAppend
    putStrLn "\nprop_foldrConcat (Should Pass)"
    quickCheck prop_foldrConcat

    -- 10)
    putStrLn "\nprop_lengthOfTake (Should Fail)"
    quickCheck prop_lengthOfTake

    -- 11)
    putStrLn "\nprop_readShow (Should Pass)"
    quickCheck prop_readShow
