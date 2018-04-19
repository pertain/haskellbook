-- endCh17.hs
--
-- End of chapter exercises (ch 17)

import Data.Monoid
--import Control.Monad (liftM2, liftM3, liftM4)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative (liftA3)
import EndCh10 (stops, vowels, nouns, verbs, xyz)


-- Specialize the types
--
-- Given a type that has instances of Applicative,
-- specialize the types of the following methods:
--      pure  :: a -> ? a
--      (<*>) :: ? (a -> b) -> ? a -> ? b

-- 1) Type: []
--
listPure :: a -> [a]
listPure = pure

listApply :: [a -> b] -> [a] -> [b]
listApply = (<*>)

-- 2) Type: IO
--
ioPure :: a -> IO a
ioPure = pure

ioApply :: IO (a -> b) -> IO a -> IO b
ioApply = (<*>)

-- 3) Type: (,) a
--
pttPure :: Monoid t => b -> (t, b)
pttPure = pure

pttApply :: Monoid t => (t, a -> b) -> (t, a) -> (t, b)
pttApply = (<*>)

-- 4) Type: (->) e
--
arPure :: a -> (e -> a)
arPure = pure

arApply :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
arApply = (<*>)


-- Write Applicative instances
--

-- 1)
data Pair a = Pair a a
    deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
    pure a = Pair a a
    (<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')

instance Arbitrary a => Arbitrary (Pair a) where
    --arbitrary = liftM2 Pair arbitrary arbitrary
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where
    (=-=) = eq

-- 2)
data Two a b = Two a b
    deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure = Two mempty
    (<*>) (Two a f) (Two a' x) = Two (a <> a') (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq

-- 3)
data Three a b c = Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure = Three mempty mempty
    (<*>) (Three a b f) (Three a' b' x)
        = Three (a <> a') (b <> b') (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
    => Arbitrary (Three a b c) where
    --arbitrary = liftM3 Three arbitrary arbitrary arbitrary
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

-- 4)
data Three' a b = Three' a b b
    deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
    pure b = Three' mempty b b
    (<*>) (Three' a f f') (Three' a' x x')
        = Three' (a <> a') (f x) (f' x')

instance (Arbitrary a, Arbitrary b)
    => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

-- 5)
data Four a b c d = Four a b c d
    deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c)
    => Applicative (Four a b c) where
    pure = Four mempty mempty mempty
    (<*>) (Four a b c f) (Four a' b' c' x)
        = Four (a <> a') (b <> b') (c <> c') (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
    => Arbitrary (Four a b c d) where
    arbitrary = 
        --liftM4 Four arbitrary arbitrary arbitrary arbitrary
        Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq

-- 6)
data Four' a b = Four' a a a b
    deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance Monoid a => Applicative (Four' a) where
    pure b = Four' mempty mempty mempty b
    (<*>) (Four' a1 a2 a3 f) (Four' a1' a2' a3' x)
        = Four' (a1 <> a1') (a2 <> a2') (a3 <> a3') (f x)

instance (Arbitrary a, Arbitrary b)
    => Arbitrary (Four' a b) where
    arbitrary =
        --liftM4 Four' arbitrary arbitrary arbitrary arbitrary
        Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq


type S = String
type ICS = (Int,Char,S)


-- Combinations
--
-- Rewrite the xyz function from chapter 10
-- using liftA3 from Control.Applicative.
-- The function generates all possible combinations
-- from three input lists
combos :: [a] -> [b] -> [c] -> [(a,b,c)]
combos xs ys zs = liftA3 (,,) xs ys zs


main :: IO ()
main = do
    putStrLn "-----------------------------------------"
    putStrLn "Pair a"
    quickBatch $ functor (undefined :: Pair ICS)
    quickBatch $ applicative (undefined :: Pair ICS)
    putStrLn "-----------------------------------------"
    putStrLn "Two a b"
    quickBatch $ functor (undefined :: Two S ICS)
    quickBatch $ applicative (undefined :: Two S ICS)
    putStrLn "-----------------------------------------"
    putStrLn "Three a b c"
    quickBatch $ functor (undefined :: Three S S ICS)
    quickBatch $ applicative (undefined :: Three S S ICS)
    putStrLn "-----------------------------------------"
    putStrLn "Three' a b"
    quickBatch $ functor (undefined :: Three' S ICS)
    quickBatch $ applicative (undefined :: Three' S ICS)
    putStrLn "-----------------------------------------"
    putStrLn "Four a b c d"
    quickBatch $ functor (undefined :: Four S S S ICS)
    quickBatch $ applicative (undefined :: Four S S S ICS)
    putStrLn "-----------------------------------------"
    putStrLn "Four' a b"
    quickBatch $ functor (undefined :: Four' S ICS)
    quickBatch $ applicative (undefined :: Four' S ICS)
    putStrLn "-----------------------------------------"
