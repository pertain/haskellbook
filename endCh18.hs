-- endCh18.hs
--
-- End of chapter exercises (ch 18)

{-# LANGUAGE FlexibleInstances #-}

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
