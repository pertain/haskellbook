-- End of chapter exercises (ch 15)


-- Semigroup exercises
--  Given a datatype, implement the semigroup instance.
--  Add semigroup constraints to type variables where needed.
--  Validate all of the instances with QuickCheck.

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

type IdentAssoc = Identity Int
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

type TwoAssoc = Two Int String
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

type ThreeAssoc = Three Int Char String
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

type FourAssoc = Four Int Char Bool String
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


main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivialAssoc)
    quickCheck (semigroupAssoc :: IdentAssoc)
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (semigroupAssoc :: ThreeAssoc)
    quickCheck (semigroupAssoc :: FourAssoc)
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
