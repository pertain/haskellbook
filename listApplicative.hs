-- listApplicative.hs
--
-- In-Chapter exercises (ch 17)

import Control.Monad (liftM2)
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- List Applicative Exercise
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
    (<*>) (Cons f fs) l@(Cons x xs) =
        Cons (f x) ((f <$> xs) <> (fs <*> l))

{-- This instance only exercises Lists up to length of 2
instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        a <- arbitrary
        frequency [(1, return $ Nil),
                   (2, return $ Cons a Nil),
                   (3, return $ Cons a (Cons a Nil))]
--}

-- This instance exercises Lists of arbitrary length
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
