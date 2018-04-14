-- ziplistApplicative.hs
--
-- In-Chapter exercises (ch 17)

import Control.Monad (liftM2)
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- ZipList Applicative Exercise
data List a = Nil | Cons a (List a)
    deriving (Eq, Show)

take' :: Int -> List a -> List a
--take' = undefined
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

instance Functor List where
    --fmap = undefined
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Monoid (List a) where
    mempty = Nil
    mappend Nil ys = ys
    mappend (Cons x xs) ys = Cons x (mappend xs ys)

instance Applicative List where
    --pure = undefined
    --(<*>) = undefined
    pure a = Cons a Nil
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Cons f fs) xs = (f <$> xs) <> (fs <*> xs)

instance Arbitrary a => Arbitrary (List a) where
    -- Generates long Lists (extremely slow)
    --arbitrary = Cons <$> arbitrary <*> arbitrary
    --
    -- Generates more reasonable List lengths (faster)
    arbitrary = sized consList
        where
            consList 0 = return Nil
            consList n =
                frequency [
                    (1, return Nil),
                    (2, liftM2 Cons arbitrary (consList (div n 2)))]

instance Eq a => EqProp (List a) where
    (=-=) = eq


newtype ZipList' a = ZipList' (List a)
    deriving (Eq, Show)

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


type ICS = (Int,Char,String)

main :: IO ()
main = do
    putStrLn "-----------------------------------------"
    putStrLn "List a"
    quickBatch $ functor (undefined :: List ICS)
    quickBatch $ applicative (undefined :: List ICS)
    putStrLn "-----------------------------------------"
    putStrLn "ZipList' a"
    quickBatch $ functor (undefined :: ZipList' ICS)
    quickBatch $ applicative (undefined :: ZipList' ICS)
    putStrLn "-----------------------------------------"
