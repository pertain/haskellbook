-- ch17mid.hs
--
-- In-Chapter exercises (ch 17)

--import Control.Applicative (liftA2)
import Data.Monoid
import Control.Monad (liftM2)
import Data.List (elemIndex)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- Exercises: Lookups
--
-- Use pure, <$>, and <*>
-- to make the expressions typecheck

-- 1)
added :: Maybe Integer
--added = (+3) (lookup 3 $ zip [1,2,3] [4,5,6])
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

-- 2)
y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
--tupled = (,) y z
tupled = (,) <$> y <*> z
--tupled = liftA2 (,) y z

-- 3)
x :: Maybe Int
x = elemIndex 3 [1,2,3,4,5]

y' :: Maybe Int
y' = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
--maxed = max' x y'
maxed = max' <$> x <*> y'
--maxed = liftA2 max' x y'

-- 4)
xs = [1,2,3]
ys = [4,5,6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
--summed = sum $ (,) x' y''
summed = sum <$> ((,) <$> x' <*> y'')


-- Exercise: Identity Instance
--
-- Write an Applicative instance for Identity
newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    --fmap = undefined
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    --pure = undefined
    --(<*>) = undefined
    pure = Identity
    Identity f <*> Identity a = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
    (=-=) = eq


-- Exercise: Constant Instance
--
-- Write an Applicative instance for Constant
newtype Constant a b = Constant {getConstant :: a}
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    --fmap = undefined
    fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    --pure = undefined
    --(<*>) = undefined
    pure _ = Constant mempty
    Constant m <*> Constant m' = Constant (m <> m')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary

instance (Eq a, Eq b) => EqProp (Constant a b) where
    (=-=) = eq


-- Exercise: Fixer Upper
--
-- Given the function and values provided,
-- use <$>, <*>, and pure to fix the broken code

--const <$> Just "Hello" <*> "World"
fixerUpper1 = const <$> Just "Hello" <*> pure "World"

--(,,,) Just 90 <*> Just 10 Just "Tierness" [1,2,3]
fixerUpper2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]


-- Exercise: List Applicative
--
-- Implement the List Applicative
data List a = Nil | Cons a (List a)
    deriving (Eq, Show)

instance Monoid (List a) where
    mempty = Nil
    mappend Nil ys = ys
    mappend (Cons x xs) ys = Cons x (mappend xs ys)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    --pure = undefined
    --(<*>) = undefined
    pure a = Cons a Nil
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Cons f fs) xs = (f <$> xs) <> (fs <*> xs)

-- Generates Lists of arbitrary length
instance Arbitrary a => Arbitrary (List a) where
    arbitrary = sized consList
        where
            consList 0 = return Nil
            consList n =
                frequency [
                    (1, return $ Nil),
                    (2, liftM2 Cons arbitrary (consList (div n 2)))]

instance Eq a => EqProp (List a) where
    (=-=) = eq

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- Write this one in terms of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
--flatMap f as = undefined
flatMap f as = concat' (fmap f as)


-- Exercise: ZipList Applicative
--
-- Implement the ZipList Applicative
newtype ZipList' a = ZipList' (List a)
    deriving (Eq, Show)

take' :: Int -> List a -> List a
--take' = undefined
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where
            xs' = let (ZipList' l) = xs
                  in take' 3000 l
            ys' = let (ZipList' l) = ys
                  in take' 3000 l

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' (fmap f xs)

-- Needed to create infinite Lists for ZipList' pure
infiniteList' :: a -> List a
infiniteList' x = xs
    where
        xs = Cons x xs

zip' :: List (a -> b) -> List a -> List b
zip' _ Nil = Nil
zip' Nil _ = Nil
zip' (Cons f fs) (Cons x xs) =
    Cons (f x) (zip' fs xs)

instance Applicative ZipList' where
    --pure = undefined
    --(<*>) = undefined
    --pure a = ZipList' (pure a)        -- single-element List (incorrect)
    pure a = ZipList' (infiniteList' a) -- infinite List (correct)
    (<*>) (ZipList' fs) (ZipList' xs) =
        ZipList' (zip' fs xs)

instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary = ZipList' <$> arbitrary


-- Exercise: Variations on Either
--
-- The Applicative instance should combine
-- failures with the Monoid typeclass
data Validation e a = Failure' e | Success' a
    deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
    --fmap f (Validation e a) = Validation e (f a)
    fmap _ (Failure' e) = Failure' e
    fmap f (Success' a) = Success' (f a)

-- this is different
instance Monoid e => Applicative (Validation e) where
    pure = Success'
    (<*>) (Success' f) (Success' x) = Success' (f x)
    (<*>) (Failure' e) (Failure' e') = Failure' (e <> e')
    (<*>) (Failure' e) _ = Failure' e
    (<*>) _ (Failure' e) = Failure' e

instance (Arbitrary a, Arbitrary e)
    => Arbitrary (Validation e a) where
    arbitrary = frequency [(3, Failure' <$> arbitrary),
                           (1, Success' <$> arbitrary)]

instance (Eq e, Eq a) => EqProp (Validation e a) where
    (=-=) = eq


type ICS = (Int,Char,String)

main :: IO ()
main = do
    putStrLn "-----------------------------------------"
    putStrLn "Identity a"
    quickBatch $ functor (undefined :: Identity ICS)
    quickBatch $ applicative (undefined :: Identity ICS)
    putStrLn "-----------------------------------------"
    putStrLn "Constant a b"
    quickBatch $ functor (undefined :: Constant String ICS)
    quickBatch $ applicative (undefined :: Constant String ICS)
    putStrLn "-----------------------------------------"
    putStrLn "List a"
    quickBatch $ monoid (undefined :: List ICS)
    quickBatch $ functor (undefined :: List ICS)
    quickBatch $ applicative (undefined :: List ICS)
    putStrLn "-----------------------------------------"
    putStrLn "ZipList' a"
    quickBatch $ functor (undefined :: ZipList' ICS)
    quickBatch $ applicative (undefined :: ZipList' ICS)
    putStrLn "-----------------------------------------"
    putStrLn "Validation e a"
    quickBatch $ functor (undefined :: Validation String ICS)
    quickBatch $ applicative (undefined :: Validation String ICS)
    putStrLn "-----------------------------------------"
