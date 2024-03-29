-- midCh16.hs
--
-- In-Chapter exercises (ch 16)

import Control.Monad (liftM, liftM2, liftM3, liftM4)
import Test.QuickCheck
import Test.QuickCheck.Function


-- Heavy Lifting exercises

-- Add fmap, parentheses, and function composition to
-- the expression as needed for the expression to
-- typecheck and produce the expected result


-- 1) expected result: a = [2]
--
-- a = (+1) $ read "[1]" :: [Int]
a = fmap (+1) $ read "[1]" :: [Int]


-- 2) expected result: b = Just ["Hi,lol", "Hellolol"]
--
--b = (++ "lol") (Just ["Hi,", "Hello"])
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])


-- 3) expected result: c 1 = -2
--
--c = (*2) (\x -> x - 2)
c = fmap (*2) (\x -> x - 2)


-- 4) expected result: d 0 = "1[0,1,2,3]"
--
--d = ((return '1' ++) . show) (\x -> [x, 1..3])
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])


-- 5) expected result: e = 3693
--
{-
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed read ("123" ++) show ioi
    in (*3) changed
-}
e :: IO Integer
e = let ioi     = readIO "1" :: IO Integer
        changed = fmap read (fmap ("123" ++) (fmap show ioi))
    in fmap (*3) changed


-- Instances of Func exercises
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b)
                                        -> (b -> c)
                                        -> f a
                                        -> Bool
functorCompose f g x =
    (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a
                                         -> Fun a b
                                         -> Fun b c
                                         -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
    (fmap (g . f) x) == (fmap g . fmap f $ x)


-- 1)
newtype Identity a = Identity a
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = liftM Identity arbitrary

type IdentityIdentity = Identity Int -> Bool

type IdentityCompose = Identity Int
                    -> Fun Int Bool
                    -> Fun Bool Int
                    -> Bool


-- 2)
data Pair a = Pair a a
    deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = liftM2 Pair arbitrary arbitrary

type PairIdentity = Pair String -> Bool

type PairCompose = Pair Char
                -> Fun Char Int
                -> Fun Int Char
                -> Bool


-- 3)
data Two a b = Two a b
    deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b)
    => Arbitrary (Two a b) where
    arbitrary = liftM2 Two arbitrary arbitrary

type TwoIdentity = Two Bool Char -> Bool

type TwoCompose = Two Int Char
               -> Fun Char String
               -> Fun String Char
               -> Bool


-- 4)
data Three a b c = Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
    => Arbitrary (Three a b c) where
    arbitrary =
        liftM3 Three arbitrary arbitrary arbitrary

type ThreeIdentity = Three Bool Char Int -> Bool

type ThreeCompose = Three String Int Char
                 -> Fun Char Bool
                 -> Fun Bool Char
                 -> Bool


-- 5)
data Three' a b = Three' a b b
    deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance (Arbitrary a, Arbitrary b)
    => Arbitrary (Three' a b) where
    arbitrary =
        liftM3 Three' arbitrary arbitrary arbitrary

type ThreeIdentity' = Three' Int Char -> Bool

type ThreeCompose' = Three' Bool Int
                  -> Fun Int Char
                  -> Fun Char Int
                  -> Bool


-- 6)
data Four a b c d = Four a b c d
    deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
    => Arbitrary (Four a b c d) where
    arbitrary =
        liftM4 Four arbitrary arbitrary arbitrary arbitrary

type FourIdentity = Four String Bool Int Char -> Bool

type FourCompose = Four (Maybe Int) String Bool Char
                -> Fun Char Int
                -> Fun Int Char
                -> Bool


-- 7)
data Four' a b = Four' a a a b
    deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (Arbitrary a, Arbitrary b)
    => Arbitrary (Four' a b) where
    arbitrary =
        liftM4 Four' arbitrary arbitrary arbitrary arbitrary

type FourIdentity' = Four' Int Char -> Bool

type FourCompose' = Four' Int (Maybe Char)
                 -> Fun (Maybe Char) Char
                 -> Fun Char (Maybe Char)
                 -> Bool


-- 8) Trivial cannot have a Functor instance,
--    because kind (* -> *) is not possible
data Trivial = Trivial


-- Exercise: Possibly
-- Write a Functor instance for Possibly
-- (a datatype identical to Maybe)
data Possibly a = LolNope | Yeppers a
    deriving (Eq, Show)

instance Functor Possibly where
    fmap _ LolNope = LolNope
    fmap f (Yeppers a) = Yeppers (f a)

instance Arbitrary a => Arbitrary (Possibly a) where
    arbitrary =
        frequency [(1, return $ LolNope),
                   (5, liftM Yeppers arbitrary)]

type PossiblyIdentity = Possibly String -> Bool

type PossiblyCompose' = Possibly Char
                     -> Fun Char Int
                     -> Fun Int Char
                     -> Bool


-- Exercise: Sum
-- Write a Functor instance for Sum
-- (a datatype identical to Either)
data Sum a b = First a | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = do
        frequency [(2, liftM First arbitrary),
                   (5, liftM Second arbitrary)]

type SumIdentity = Sum String Int -> Bool

type SumCompose' = Sum Int (Maybe Char)
                -> Fun (Maybe Char) Int
                -> Fun Int (Maybe Char)
                -> Bool


runQc :: IO ()
runQc = do
    putStrLn "----------------------------"
    putStrLn "Identity a"
    quickCheck (functorIdentity :: IdentityIdentity)
    quickCheck (functorCompose' :: IdentityCompose)
    putStrLn "Pair a"
    quickCheck (functorIdentity :: PairIdentity)
    quickCheck (functorCompose' :: PairCompose)
    putStrLn "Two a b"
    quickCheck (functorIdentity :: TwoIdentity)
    quickCheck (functorCompose' :: TwoCompose)
    putStrLn "Three a b c"
    quickCheck (functorIdentity :: ThreeIdentity)
    quickCheck (functorCompose' :: ThreeCompose)
    putStrLn "Three' a b"
    quickCheck (functorIdentity :: ThreeIdentity')
    quickCheck (functorCompose' :: ThreeCompose')
    putStrLn "Four a b c d"
    quickCheck (functorIdentity :: FourIdentity)
    quickCheck (functorCompose' :: FourCompose)
    putStrLn "Four' a b"
    quickCheck (functorIdentity :: FourIdentity')
    quickCheck (functorCompose' :: FourCompose')
    putStrLn "----------------------------"
    putStrLn "Possibly a (clone of Maybe)"
    quickCheck (functorIdentity :: PossiblyIdentity)
    --verboseCheck (functorIdentity :: PossiblyIdentity)
    quickCheck (functorCompose' :: PossiblyCompose')
    putStrLn "----------------------------"
    putStrLn "Sum a b (clone of Either)"
    quickCheck (functorIdentity :: SumIdentity)
    --verboseCheck (functorIdentity :: SumIdentity)
    quickCheck (functorCompose' :: SumCompose')
