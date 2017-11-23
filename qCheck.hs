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
            go y (Just x, t) = (Just y, x >= y)

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


runQc :: IO ()
runQc = do
    quickCheck prop_doubleThenHalve
    quickCheck prop_sortedIsSorted
    quickCheck prop_associativeAddition
    quickCheck prop_commutativeAddition
