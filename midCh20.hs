-- midCh20.hs
--
-- In-chapter exercises (ch 20)

import Data.Foldable
import Data.Monoid hiding ((<>))
import Data.Semigroup
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Exercises: Library Functions
--
-- Implement the functions in terms
-- of fmap or foldr from Foldable

-- 1) sum

-- foldMap
sumF :: (Foldable t, Num a) => t a -> a
sumF = getSum . (foldMap Sum)

-- foldr
sumF' :: (Foldable t, Num a) => t a -> a
sumF' = foldr (+) 0


-- 2) product

-- foldMap
productF :: (Foldable t, Num a) => t a -> a
productF = getProduct . (foldMap Product)

-- foldr
productF' :: (Foldable t, Num a) => t a -> a
productF' = foldr (*) 1


-- 3) elem

-- foldMap
elemF :: (Foldable t, Eq a) => a -> t a -> Bool
elemF a = getAny . (foldMap (Any . (== a)))

-- foldr
elemF' :: (Foldable t, Eq a) => a -> t a -> Bool
elemF' a = foldr (\x y -> y || (a == x)) False


-- 4) minimum
newtype MinM a = MinM {getMinM :: Maybe a}
    deriving (Eq, Show)

instance Ord a => Monoid (MinM a) where
    mempty = MinM Nothing
    mappend a (MinM Nothing) = a
    mappend (MinM Nothing) a = a
    mappend (MinM a) (MinM a') = MinM (min a a')

instance Ord a => Semigroup (MinM a) where
    (<>) = mappend

instance Arbitrary a => Arbitrary (MinM a) where
    arbitrary = MinM <$> arbitrary

instance Eq a => EqProp (MinM a) where
    (=-=) = eq

-- foldMap
minimumF :: (Foldable t, Ord a) => t a -> Maybe a
minimumF = getMinM . (foldMap (MinM . pure))

-- foldr
minimumF' :: (Foldable t, Ord a) => t a -> Maybe a
minimumF' = foldr go Nothing
    where
        go x y =
            case y of
                Nothing -> Just x
                Just a  -> Just (min x a)


-- 5) maximum
newtype MaxM a = MaxM {getMaxM :: Maybe a}
    deriving (Eq, Show)

instance Ord a => Monoid (MaxM a) where
    mempty = MaxM Nothing
    mappend a (MaxM Nothing) = a
    mappend (MaxM Nothing) a = a
    mappend (MaxM a) (MaxM a') = MaxM (max a a')

instance Ord a => Semigroup (MaxM a) where
    (<>) = mappend

instance Arbitrary a => Arbitrary (MaxM a) where
    arbitrary = MaxM <$> arbitrary

instance Eq a => EqProp (MaxM a) where
    (=-=) = eq

-- foldMap
maximumF :: (Foldable t, Ord a) => t a -> Maybe a
maximumF = getMaxM . (foldMap (MaxM . pure))

-- foldr
maximumF' :: (Foldable t, Ord a) => t a -> Maybe a
maximumF' = foldr go Nothing
    where
        go x y =
            case y of
                Nothing -> Just x
                Just a  -> Just (max x a)


-- 6) null

-- foldMap
nullF :: Foldable t => t a -> Bool
nullF = getAll . (foldMap (const (All False)))

-- foldr
nullF' :: Foldable t => t a -> Bool
nullF' = foldr (\_ _ -> False) True


-- 7) length

-- foldMap
lengthF :: Foldable t => t a -> Int
lengthF = getSum . (foldMap (const (Sum 1)))

-- foldr
lengthF' :: Foldable t => t a -> Int
lengthF' = foldr (const (+1)) 0


-- 8) toList

-- foldMap
toListF :: Foldable t => t a -> [a]
toListF = foldMap (: [])

-- foldr
toListF' :: Foldable t => t a -> [a]
toListF' = foldr (:) []


-- 9) Implement fold in terms of foldMap
foldF :: (Foldable t, Monoid m) => t m -> m
foldF = foldMap . mappend $ mempty


-- 10) Implement foldMap in terms of foldr
--
-- Define foldMap in terms of foldr
foldMapF :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMapF f = foldr (mappend . f) mempty


type ICS = (Int,Char,String)

main :: IO ()
main = do
    putStrLn "-----------------------------------------"
    putStrLn "MinM a"
    quickBatch $ monoid (undefined :: MinM ICS)
    putStrLn "\nMaxM a"
    quickBatch $ monoid (undefined :: MaxM ICS)
    putStrLn "-----------------------------------------"
