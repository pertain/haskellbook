-- midCh20.hs
--
-- In-chapter exercises (ch 20)

import Data.Foldable
import Data.Monoid

-- Exercises: Library Functions
--
-- Implement the functions in terms
-- of fmap or foldr from Foldable

-- 1) sum
sumF :: (Foldable t, Num a) => t a -> a
sumF = getSum . (foldMap Sum)

-- 2) product
productF :: (Foldable t, Num a) => t a -> a
productF = getProduct . (foldMap Product)

-- 3) elem
elemF :: (Foldable t, Eq a) => a -> t a -> Bool
--elemF a ta = getAny $ foldMap (Any . (== a)) ta
elemF a = getAny . (foldMap (Any . (== a)))

-- 4) minimum
minimumF :: (Foldable t, Ord a) => t a -> Bool
minimumF = undefined
