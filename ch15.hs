{-# LANGUAGE ViewPatterns #-}

-- End of chapter exercises (ch 15)


-- Semigroup exercises
--  Given a datatype, implement the semigroup instance.
--  Add semigroup constraints to type variables where needed.
--  Validate all of the instances with QuickCheck.

import Control.Monad
import Data.Semigroup
import Test.QuickCheck

-- 1)
data Trivial = Trivial
    deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial
                 -> Trivial
                 -> Trivial
                 -> Bool


-- 2)
newtype Identity a = Identity a
    deriving (Eq, Show)

instance Semigroup (Identity a) where
    x <> _ = x

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return (Identity a)

type IdentAssocInt = Identity Int
                  -> Identity Int
                  -> Identity Int
                  -> Bool

-- 3)
data Two a b = Two a b
    deriving (Eq, Show)

instance Semigroup (Two a b) where
    x <> _ = x

instance (Arbitrary a, Arbitrary b)
    => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Two a b)

type TwoAssocIntString = Two Int String
                      -> Two Int String
                      -> Two Int String
                      -> Bool

-- 4)
data Three a b c = Three a b c
    deriving (Eq, Show)

instance Semigroup (Three a b c) where
    x <> _ = x

instance (Arbitrary a, Arbitrary b, Arbitrary c)
    => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (Three a b c)

type ThreeAssocIntCharString = Three Int Char String
                            -> Three Int Char String
                            -> Three Int Char String
                            -> Bool

-- 5)
data Four a b c d = Four a b c d
    deriving (Eq, Show)

instance Semigroup (Four a b c d) where
    x <> _ = x

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
    => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return (Four a b c d)

type FourAssocIntCharBoolString = Four Int Char Bool String
                               -> Four Int Char Bool String
                               -> Four Int Char Bool String
                               -> Bool


-- 6)
newtype BoolConj = BoolConj Bool
    deriving (Eq, Show)

instance Semigroup BoolConj where
    BoolConj True <> BoolConj True = BoolConj True
    _ <> _ = BoolConj False

instance Arbitrary BoolConj where
    arbitrary =
        frequency [ (3, return $ BoolConj True)
                  , (1, return $ BoolConj False) ]

type BoolConjAssoc = BoolConj
                  -> BoolConj
                  -> BoolConj
                  -> Bool


-- 7)
newtype BoolDisj = BoolDisj Bool
    deriving (Eq, Show)

instance Semigroup BoolDisj where
    BoolDisj False <> BoolDisj False = BoolDisj False
    _ <> _ = BoolDisj True

instance Arbitrary BoolDisj where
    arbitrary =
        frequency [ (3, return $ BoolDisj True)
                  , (1, return $ BoolDisj False) ]

type BoolDisjAssoc = BoolDisj
                  -> BoolDisj
                  -> BoolDisj
                  -> Bool


-- 8)
data Or a b = Fst a | Snd b
    deriving (Eq, Show)

instance Semigroup (Or a b) where
    x@(Snd _) <> _ = x
    _ <> x = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [ (return $ Fst a)
              , (return $ Snd b) ]

type OrAssocIntBool = Or Int Bool
                   -> Or Int Bool
                   -> Or Int Bool
                   -> Bool


-- 9)
newtype Combine a b = Combine { unCombine :: (a -> b) }

-- Generic Show instance for function (a -> b)
instance (Show a, Show b) => Show (Combine a b) where
    show _ = "function: (Combine a b)"

instance Semigroup b => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine (f <> g)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    --arbitrary = do
        --f <- arbitrary
        --return $ Combine f
    arbitrary = liftM Combine arbitrary

prop_combAssoc :: (Integral a, Eq b, Semigroup b) => Combine a b
                                                  -> Combine a b
                                                  -> Combine a b
                                                  -> a
                                                  -> Bool
prop_combAssoc f g h n =
    unCombine (f <> (g <> h)) (n) == unCombine ((f <> g) <> h) (n)

type CombAssocIntSumInt = Combine Int (Sum Int)
                       -> Combine Int (Sum Int)
                       -> Combine Int (Sum Int)
                       -> Int
                       -> Bool

type CombAssocIntListInt = Combine Int [Int]
                        -> Combine Int [Int]
                        -> Combine Int [Int]
                        -> Int
                        -> Bool

type CombAssocIntListDouble = Combine Int [Double]
                           -> Combine Int [Double]
                           -> Combine Int [Double]
                           -> Int
                           -> Bool

type CombAssocIntProductInt = Combine Int (Product Int)
                           -> Combine Int (Product Int)
                           -> Combine Int (Product Int)
                           -> Int
                           -> Bool


main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivialAssoc)
    quickCheck (semigroupAssoc :: IdentAssocInt)
    quickCheck (semigroupAssoc :: TwoAssocIntString)
    quickCheck (semigroupAssoc :: ThreeAssocIntCharString)
    quickCheck (semigroupAssoc :: FourAssocIntCharBoolString)
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (semigroupAssoc :: OrAssocIntBool)
    -- 9)
    --quickCheck (semigroupAssoc :: CombAssocIntSumInt)
    quickCheck (prop_combAssoc :: CombAssocIntSumInt)
    quickCheck (prop_combAssoc :: CombAssocIntListInt)
    quickCheck (prop_combAssoc :: CombAssocIntListDouble)
    quickCheck (prop_combAssoc :: CombAssocIntProductInt)
