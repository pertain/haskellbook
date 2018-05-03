-- midCh20.hs
--
-- In-chapter exercises (ch 20)

import Data.Foldable
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

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
data Min a = Min {getMin :: Maybe a}
    deriving (Eq, Show)

instance Ord a => Monoid (Min a) where
    mempty = Min Nothing
    mappend a (Min Nothing) = a
    mappend (Min Nothing) a = a
    mappend (Min a) (Min a') = Min (min a a')

instance Arbitrary a => Arbitrary (Min a) where
    arbitrary = Min <$> arbitrary

instance Eq a => EqProp (Min a) where
    (=-=) = eq

minimumF :: (Foldable t, Ord a) => t a -> Maybe a
minimumF = getMin . (foldMap (Min . pure))


-- 5) maximum
data Max a = Max {getMax :: Maybe a}
    deriving (Eq, Show)

instance Ord a => Monoid (Max a) where
    mempty = Max Nothing
    mappend a (Max Nothing) = a
    mappend (Max Nothing) a = a
    mappend (Max a) (Max a') = Max (max a a')

instance Arbitrary a => Arbitrary (Max a) where
    arbitrary = Max <$> arbitrary

instance Eq a => EqProp (Max a) where
    (=-=) = eq

maximumF :: (Foldable t, Ord a) => t a -> Maybe a
maximumF = getMax . (foldMap (Max . pure))


-- 6) null
data Null = Null {getNull :: Bool}
    deriving (Eq, Show)

instance Monoid Null where
    mempty = Null True
    mappend a (Null True) = a
    mappend (Null True) a = a
    mappend _ _ = Null False

instance Arbitrary Null where
    arbitrary = Null <$> arbitrary

instance EqProp Null where
    (=-=) = eq

nullF :: Foldable t => t a -> Bool
nullF = undefined


type ICS = (Int,Char,String)

main :: IO ()
main = do
    putStrLn "-----------------------------------------"
    putStrLn "Min a"
    quickBatch $ monoid (undefined :: Min ICS)
    putStrLn "Max a"
    quickBatch $ monoid (undefined :: Max ICS)
    putStrLn "Null a"
    quickBatch $ monoid (undefined :: Null)
    putStrLn "-----------------------------------------"
