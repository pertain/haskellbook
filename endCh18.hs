-- endCh18.hs
--
-- End of chapter exercises (ch 18)

{-# LANGUAGE FlexibleInstances #-}

import Data.Monoid
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- Write Monad instances for the following types
--

-- 1)
data Nope a = NopeDotJpg
    deriving (Eq, Show)

instance Functor Nope where
    fmap _ _  = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    (<*>) _ _ = NopeDotJpg

instance Monad Nope where
    return = pure
    (>>=) NopeDotJpg _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
    (=-=) = eq


-- 2)
data PhhhbbtttEither b a = Left' a | Right' b
    deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
    fmap _ (Right' b) = Right' b
    fmap f (Left' t) = Left' (f t)

instance Applicative (PhhhbbtttEither b) where
    pure t = Left' t
    (<*>) (Right' b) _ = Right' b
    (<*>) _ (Right' b) = Right' b
    (<*>) (Left' f) (Left' t) = Left' (f t)

instance Monad (PhhhbbtttEither b) where
    return = pure
    (>>=) (Right' b) _ = (Right' b)
    (>>=) (Left' t) f = f t

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
    arbitrary = frequency [(1, Right' <$> arbitrary),
                           (3, Left' <$> arbitrary)]

instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where
    (=-=) = eq


-- 3)
newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
    return = pure
    (>>=) (Identity a) f = f a

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
    (=-=) = eq


-- 4)
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
    pure a = Cons a Nil
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Cons f fs) xs = (f <$> xs) <> (fs <*> xs)

instance Monad List where
    return = pure
    (>>=) Nil _ = Nil
    (>>=) (Cons x xs) f = f x <> (xs >>= f)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = sized consList
        where
            consList 0 = return Nil
            consList n =
                frequency [
                    (1, return Nil),
                    (2, Cons <$> arbitrary <*> consList (div n 2))]

instance Eq a => EqProp (List a) where
    (=-=) = eq


-- Write the following functions using methods
-- provided by Monad and Functor (can use stuff
-- like identity and composition, but the types
-- provided must typecheck as is)
--

-- 1)
j :: Monad m => m (m a) -> m a
j = join

-- 2)
l1 :: Monad m => (a -> b) -> m a -> m b
--l1 = fmap
l1 = liftM

-- 3)
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = f <$> ma <*> mb
--l2 = liftM2

-- 4)
a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

-- 5)
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh = undefined

-- 6)
flipType :: (Monad m) => [m a] -> m [a]
flipType = undefined


type ICS = (Int,Char,String)
type S = String

main :: IO ()
main = do
    putStrLn "-----------------------------------------"
    putStrLn "Nope a"
    quickBatch $ functor (undefined :: Nope ICS)
    quickBatch $ applicative (undefined :: Nope ICS)
    quickBatch $ monad (undefined :: Nope ICS)
    putStrLn "-----------------------------------------"
    putStrLn "PhhhbbtttEither b a"
    quickBatch $ functor (undefined :: PhhhbbtttEither S ICS)
    quickBatch $ applicative (undefined :: PhhhbbtttEither S ICS)
    quickBatch $ monad (undefined :: PhhhbbtttEither S ICS)
    putStrLn "-----------------------------------------"
    putStrLn "Identity a"
    quickBatch $ functor (undefined :: Identity ICS)
    quickBatch $ applicative (undefined :: Identity ICS)
    quickBatch $ monad (undefined :: Identity ICS)
    putStrLn "-----------------------------------------"
    putStrLn "List a"
    quickBatch $ monoid (undefined :: List ICS)
    quickBatch $ functor (undefined :: List ICS)
    quickBatch $ applicative (undefined :: List ICS)
    quickBatch $ monad (undefined :: List ICS)
    putStrLn "-----------------------------------------"
