{-# LANGUAGE ViewPatterns #-}

-- End of chapter exercises (ch 15)


-- Semigroup exercises
--  Given a datatype, implement the semigroup instance.
--  Add semigroup constraints to type variables where needed.
--  Validate all of the instances with QuickCheck.

import Control.Monad (liftM, liftM2, liftM3, liftM4)
import Data.Semigroup
import Test.QuickCheck hiding (Success, Failure)

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
    --arbitrary = do
        --a <- arbitrary
        --return (Identity a)
    arbitrary =
        liftM Identity arbitrary

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
    --arbitrary = do
        --a <- arbitrary
        --b <- arbitrary
        --return (Two a b)
    arbitrary =
        liftM2 Two arbitrary arbitrary

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
    --arbitrary = do
        --a <- arbitrary
        --b <- arbitrary
        --c <- arbitrary
        --return (Three a b c)
    arbitrary =
        liftM3 Three arbitrary arbitrary arbitrary

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
    --arbitrary = do
        --a <- arbitrary
        --b <- arbitrary
        --c <- arbitrary
        --d <- arbitrary
        --return (Four a b c d)
    arbitrary =
        liftM4 Four arbitrary arbitrary arbitrary arbitrary

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
    --arbitrary = do
        --a <- arbitrary
        --b <- arbitrary
        --oneof [ (return $ Fst a)
              --, (return $ Snd b) ]
    arbitrary =
        oneof [ (liftM Fst arbitrary)
              , (liftM Snd arbitrary) ]

type OrAssocIntBool = Or Int Bool
                   -> Or Int Bool
                   -> Or Int Bool
                   -> Bool


-- 9)
newtype Combine a b = Combine { unCombine :: (a -> b) }

-- Generic Show instance for function (a -> b)
instance Show (Combine a b) where
    show _ = "function: (Combine a b)"

instance Semigroup b => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine (f <> g)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    --arbitrary = do
        --f <- arbitrary
        --return $ Combine f
    arbitrary = liftM Combine arbitrary

--prop_combAssoc :: (Integral a, Eq b, Semigroup b) => Combine a b
prop_combAssoc :: (Eq b, Semigroup b) => Combine a b
                                      -> Combine a b
                                      -> Combine a b
                                      -> a
                                      -> Bool
prop_combAssoc f g h a =
    unCombine (f <> (g <> h)) (a) == unCombine ((f <> g) <> h) (a)

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

type CombAssocStringMaybeString = Combine String (Maybe String)
                               -> Combine String (Maybe String)
                               -> Combine String (Maybe String)
                               -> String
                               -> Bool


-- 10)
newtype Comp a = Comp { unComp :: (a -> a) }

-- Generic Show instance for function (a -> b)
instance Show (Comp a) where
    show _ = "function: (Comp a)"

instance Semigroup a => Semigroup (Comp a) where
    (Comp f) <> (Comp g) = Comp (f <> g)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
    --arbitrary = do
        --f <- arbitrary
        --return $ Comp f
    arbitrary = liftM Comp arbitrary

prop_compAssoc :: (Eq a, Semigroup a) => Comp a
                                      -> Comp a
                                      -> Comp a
                                      -> a
                                      -> Bool
prop_compAssoc f g h a =
    unComp ((f <> g) <> h) (a) == unComp (f <> (g <> h)) (a)

type CompAssocString = Comp String
                    -> Comp String
                    -> Comp String
                    -> String
                    -> Bool

type CompAssocSumInt = Comp (Sum Int)
                    -> Comp (Sum Int)
                    -> Comp (Sum Int)
                    -> Sum Int
                    -> Bool


-- 11)
data Validation a b = Failure a | Success b
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    x@(Success _) <> _ = x
    _ <> x@(Success _) = x
    Failure x <> Failure y = Failure (x <> y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    --arbitrary = do
        --a <- arbitrary
        --b <- arbitrary
        --oneof [ (return $ Failure a)
              --, (return $ Success b) ]
    arbitrary =
        oneof [ (liftM Failure arbitrary)
              , (liftM Success arbitrary) ]

type ValStringBool = Validation String Bool
                  -> Validation String Bool
                  -> Validation String Bool
                  -> Bool


runQc :: IO ()
runQc = do
    putStrLn "Trivial"
    quickCheck (semigroupAssoc :: TrivialAssoc)
    putStrLn "Identity"
    quickCheck (semigroupAssoc :: IdentAssocInt)
    putStrLn "Two a b"
    quickCheck (semigroupAssoc :: TwoAssocIntString)
    putStrLn "Three a b c"
    quickCheck (semigroupAssoc :: ThreeAssocIntCharString)
    putStrLn "Four a b c d"
    quickCheck (semigroupAssoc :: FourAssocIntCharBoolString)
    putStrLn "BoolConj"
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    putStrLn "BoolDisj"
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    putStrLn "Or a b"
    quickCheck (semigroupAssoc :: OrAssocIntBool)
    -- 9)
    putStrLn "Combine a b"
    --quickCheck (semigroupAssoc :: CombAssocIntSumInt)
    quickCheck (prop_combAssoc :: CombAssocIntSumInt)
    quickCheck (prop_combAssoc :: CombAssocIntListInt)
    quickCheck (prop_combAssoc :: CombAssocIntListDouble)
    quickCheck (prop_combAssoc :: CombAssocIntProductInt)
    quickCheck (prop_combAssoc :: CombAssocStringMaybeString)
    -- 10)
    putStrLn "Comp a"
    quickCheck (prop_compAssoc :: CompAssocString)
    quickCheck (prop_compAssoc :: CompAssocSumInt)
    -- 11)
    putStrLn "Validation a b"
    quickCheck (semigroupAssoc :: ValStringBool)


-- 11)
main :: IO ()
main = do
    let failure :: String -> Validation String Int
        failure = Failure
        success :: Int -> Validation String Int
        success = Success
    print $ success 1 <> failure "blah"
    print $ failure "woot" <> failure "blah"
    print $ success 1 <> success 2
    print $ failure "woot" <> success 2
