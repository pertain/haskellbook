{-# LANGUAGE ViewPatterns #-}

-- End of chapter exercises (ch 15)

import Control.Monad (liftM, liftM2, liftM3, liftM4)
import Data.Semigroup
import Test.QuickCheck hiding (Success, Failure)


------------------------
-- Semigroup exercises
------------------------
-- Given a datatype, implement the Semigroup instance.
-- Add Semigroup constraints to type variables where needed.
-- Validate all of the instances with QuickCheck.

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc x y z = (x <> (y <> z)) == ((x <> y) <> z)

-- 1)
data Trivial = Trivial
    deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivialAssoc = Trivial
                 -> Trivial
                 -> Trivial
                 -> Bool


-- 2)
newtype Identity a = Identity a
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    Identity x <> Identity y = Identity (x <> y)
    

instance Arbitrary a => Arbitrary (Identity a) where
    --arbitrary = do
        --x <- arbitrary
        --return (Identity x)
    arbitrary =
        liftM Identity arbitrary

type IdentityAssocString = Identity String
                        -> Identity String
                        -> Identity String
                        -> Bool

-- 3)
data Two a b = Two a b
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    Two x y <> Two x' y' = Two (x <> x') (y <> y')

instance (Arbitrary a, Arbitrary b)
    => Arbitrary (Two a b) where
    --arbitrary = do
        --x <- arbitrary
        --y <- arbitrary
        --return (Two x y)
    arbitrary =
        liftM2 Two arbitrary arbitrary

type TwoAssocStringString = Two String String
                         -> Two String String
                         -> Two String String
                         -> Bool

-- 4)
data Three a b c = Three a b c
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c)
    => Semigroup (Three a b c) where
    Three x y z <> Three x' y' z' = Three (x <> x')
                                          (y <> y')
                                          (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c)
    => Arbitrary (Three a b c) where
    --arbitrary = do
        --x <- arbitrary
        --y <- arbitrary
        --z <- arbitrary
        --return (Three x y z)
    arbitrary =
        liftM3 Three arbitrary arbitrary arbitrary

type ThreeAssocSumProdString = Three (Sum Int) (Product Int) String
                            -> Three (Sum Int) (Product Int) String
                            -> Three (Sum Int) (Product Int) String
                            -> Bool

-- 5)
data Four a b c d = Four a b c d
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
    => Semigroup (Four a b c d) where
    Four w x y z <> Four w' x' y' z' = Four (w <> w')
                                            (x <> x')
                                            (y <> y')
                                            (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
    => Arbitrary (Four a b c d) where
    --arbitrary = do
        --w <- arbitrary
        --x <- arbitrary
        --y <- arbitrary
        --z <- arbitrary
        --return (Four w x y z)
    arbitrary =
        liftM4 Four arbitrary arbitrary arbitrary arbitrary

type FourAssocStringSumTwoString = Four String
                                        (Sum Int)
                                        (Two (Product Int) String)
                                        String
                                -> Four String
                                        (Sum Int)
                                        (Two (Product Int) String)
                                        String
                                -> Four String
                                        (Sum Int)
                                        (Two (Product Int) String)
                                        String
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
        --x <- arbitrary
        --y <- arbitrary
        --oneof [ (return $ Fst x)
              --, (return $ Snd y) ]
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
    Combine f <> Combine g = Combine (f <> g)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    --arbitrary = do
        --f <- arbitrary
        --return $ Combine f
    arbitrary = liftM Combine arbitrary

semigroupCombineAssoc :: (Eq b, Semigroup b) => Combine a b
                                             -> Combine a b
                                             -> Combine a b
                                             -> a
                                             -> Bool
semigroupCombineAssoc f g h x =
    unCombine (f <> (g <> h)) x == unCombine ((f <> g) <> h) x

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
    Comp f <> Comp g = Comp (f <> g)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
    --arbitrary = do
        --f <- arbitrary
        --return $ Comp f
    arbitrary = liftM Comp arbitrary

semigroupCompAssoc :: (Eq a, Semigroup a) => Comp a
                                          -> Comp a
                                          -> Comp a
                                          -> a
                                          -> Bool
semigroupCompAssoc f g h x =
    unComp ((f <> g) <> h) (x) == unComp (f <> (g <> h)) (x)

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
        --x <- arbitrary
        --y <- arbitrary
        --oneof [ (return $ Failure x)
              --, (return $ Success y) ]
    arbitrary =
        oneof [ (liftM Failure arbitrary)
              , (liftM Success arbitrary) ]

type ValStringBool = Validation String Bool
                  -> Validation String Bool
                  -> Validation String Bool
                  -> Bool


------------------------
-- Monoid exercises
------------------------
-- Given a datatype, implement the Monoid instance
-- Add Monoid constraints to type variables where needed.
-- Validate all of the instances with QuickCheck.

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity x = mempty `mappend` x == x

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity x = x `mappend` mempty == x


-- 1) Trivial
instance Monoid Trivial where
    mempty = Trivial
    _ `mappend` _ = Trivial

type TrivialIdentity = Trivial -> Bool


-- 2) Identity a
instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty
    Identity x `mappend` Identity y = Identity (x `mappend` y)

type IdentityIdentityString = Identity String -> Bool


-- 3) Two a b
--    Three a b c
--    Four a b c d
instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    Two x y `mappend` Two x' y' = Two (x `mappend` x')
                                      (y `mappend` y')

type TwoIdentitySumString = Two (Sum Int) String -> Bool

instance (Monoid a, Monoid b, Monoid c)
    => Monoid (Three a b c) where
    mempty = Three mempty mempty mempty
    Three x y z `mappend` Three x' y' z' = Three (x `mappend` x')
                                                 (y `mappend` y')
                                                 (z `mappend` z')

type ThreeIdentitySumSumProd
    = Three (Sum Int) (Sum Int) (Product Int) -> Bool

instance (Monoid a, Monoid b, Monoid c, Monoid d)
    => Monoid (Four a b c d) where
    mempty = Four mempty mempty mempty mempty
    Four w x y z `mappend` Four w' x' y' z' = Four (w `mappend` w')
                                                   (x `mappend` x')
                                                   (y `mappend` y')
                                                   (z `mappend` z')

type FourIdentityStringStringSumSum
    = Four String String (Sum Int) (Sum Int) -> Bool


-- 4) BoolConj
instance Monoid BoolConj where
    mempty = BoolConj True
    BoolConj True `mappend` BoolConj True = BoolConj True
    _ `mappend` _ = BoolConj False

type BoolConjIdentity = BoolConj -> Bool


-- 5) BoolDisj
instance Monoid BoolDisj where
    mempty = BoolDisj False
    BoolDisj False `mappend` BoolDisj False = BoolDisj False
    _ `mappend` _ = BoolDisj True

type BoolDisjIdentity = BoolDisj -> Bool


-- 6) Combine a b
instance Monoid b => Monoid (Combine a b) where
    mempty = Combine mempty
    Combine f `mappend` Combine g = Combine (f `mappend` g)

monoidCombineLeftIdentity :: (Eq b, Monoid b) => Combine a b -> a -> Bool
monoidCombineLeftIdentity f x =
    unCombine (f `mappend` mempty) x == unCombine f x

monoidCombineRightIdentity :: (Eq b, Monoid b) => Combine a b -> a -> Bool
monoidCombineRightIdentity f x =
    unCombine (mempty `mappend` f) x == unCombine f x

type CombIdentityIntSum = Combine Int (Sum Int) -> Int -> Bool


-- 7) Comp a
instance Monoid a => Monoid (Comp a) where
    mempty = Comp mempty
    Comp f `mappend` Comp g = Comp (f `mappend` g)

monoidCompLeftIdentity :: (Eq a, Monoid a) => Comp a -> a -> Bool
monoidCompLeftIdentity f x =
    unComp (f `mappend` mempty) x == unComp f x

monoidCompRightIdentity :: (Eq a, Monoid a) => Comp a -> a -> Bool
monoidCompRightIdentity f x =
    unComp (mempty `mappend` f) x == unComp f x

type CompIdentityProd = Comp (Product Int) -> Product Int -> Bool


-- 8)
newtype Mem s a = Mem { runMem :: s -> (a, s) }

-- Generic Show instance for function (s -> (a, s))
instance Show (Mem s a) where
    show _ = "function: (Mem s a)"

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem (\s -> (mempty, s))
    Mem f `mappend` Mem g = Mem (go f g)
        where
            go f' g' x = (fst (f' x) `mappend` fst (g' x), snd (f' (snd (g' x))))

instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (Mem s a) where
    --arbitrary = do
        --f <- arbitrary
        --return $ Combine f
    arbitrary = liftM Mem arbitrary

monoidMemAssoc :: (Eq s, Eq a, Monoid a) => Mem s a -> Mem s a -> Mem s a -> s -> Bool
monoidMemAssoc f g h x =
    runMem (f `mappend` (g `mappend` h)) x == runMem ((f `mappend` g) `mappend` h) x

monoidMemLeftIdentity :: (Eq s, Eq a, Monoid a) => Mem s a -> s -> Bool
monoidMemLeftIdentity f x =
    runMem (f `mappend` mempty) x == runMem f x

monoidMemRightIdentity :: (Eq s, Eq a, Monoid a) => Mem s a -> s -> Bool
monoidMemRightIdentity f x =
    runMem (mempty `mappend` f) x == runMem f x

type MemAssocIntString = Mem Int String
                      -> Mem Int String
                      -> Mem Int String
                      -> Int
                      -> Bool

type MemIdentityIntString = Mem Int String
                         -> Int
                         -> Bool

fun' :: Mem Int String
fun' = Mem $ \s -> ("hi", s + 1)


runQc :: IO ()
runQc = do
    -- Semigroup
    putStrLn "\nSemigroup"
    putStrLn "-------------------------"
    putStrLn "Trivial"
    quickCheck (semigroupAssoc :: TrivialAssoc)
    putStrLn "Identity a"
    quickCheck (semigroupAssoc :: IdentityAssocString)
    putStrLn "Two a b"
    quickCheck (semigroupAssoc :: TwoAssocStringString)
    putStrLn "Three a b c"
    quickCheck (semigroupAssoc :: ThreeAssocSumProdString)
    putStrLn "Four a b c d"
    quickCheck (semigroupAssoc :: FourAssocStringSumTwoString)
    putStrLn "BoolConj"
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    putStrLn "BoolDisj"
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    putStrLn "Or a b"
    quickCheck (semigroupAssoc :: OrAssocIntBool)
    putStrLn "Combine a b"
    quickCheck (semigroupCombineAssoc :: CombAssocIntSumInt)
    quickCheck (semigroupCombineAssoc :: CombAssocIntListInt)
    quickCheck (semigroupCombineAssoc :: CombAssocIntListDouble)
    quickCheck (semigroupCombineAssoc :: CombAssocIntProductInt)
    quickCheck (semigroupCombineAssoc :: CombAssocStringMaybeString)
    putStrLn "Comp a"
    quickCheck (semigroupCompAssoc :: CompAssocString)
    quickCheck (semigroupCompAssoc :: CompAssocSumInt)
    putStrLn "Validation a b"
    quickCheck (semigroupAssoc :: ValStringBool)

    -- Monoid
    putStrLn "\nMonoid"
    putStrLn "-------------------------"
    putStrLn "Trivial"
    quickCheck (monoidLeftIdentity :: TrivialIdentity)
    quickCheck (monoidRightIdentity :: TrivialIdentity)
    putStrLn "Identity a"
    quickCheck (monoidLeftIdentity :: IdentityIdentityString)
    quickCheck (monoidRightIdentity :: IdentityIdentityString)
    putStrLn "Two a b"
    quickCheck (monoidLeftIdentity :: TwoIdentitySumString)
    quickCheck (monoidRightIdentity :: TwoIdentitySumString)
    putStrLn "Three a b c"
    quickCheck (monoidLeftIdentity :: ThreeIdentitySumSumProd)
    quickCheck (monoidRightIdentity :: ThreeIdentitySumSumProd)
    putStrLn "Four a b c d"
    quickCheck (monoidLeftIdentity :: FourIdentityStringStringSumSum)
    quickCheck (monoidRightIdentity :: FourIdentityStringStringSumSum)
    putStrLn "BoolConj"
    quickCheck (monoidLeftIdentity :: BoolConjIdentity)
    quickCheck (monoidRightIdentity :: BoolConjIdentity)
    putStrLn "BoolDisj"
    quickCheck (monoidLeftIdentity :: BoolDisjIdentity)
    quickCheck (monoidRightIdentity :: BoolDisjIdentity)
    putStrLn "Combine a b"
    quickCheck (monoidCombineLeftIdentity :: CombIdentityIntSum)
    quickCheck (monoidCombineRightIdentity :: CombIdentityIntSum)
    putStrLn "Comp a"
    quickCheck (monoidCompLeftIdentity :: CompIdentityProd)
    quickCheck (monoidCompRightIdentity :: CompIdentityProd)
    putStrLn "Mem s a"
    quickCheck (monoidMemAssoc :: MemAssocIntString)
    quickCheck (monoidMemLeftIdentity :: MemIdentityIntString)
    quickCheck (monoidMemRightIdentity :: MemIdentityIntString)


main :: IO ()
main = do
    -- Semigroup (11)
    putStrLn "\nSemigroup -- exercise 11"
    let failure :: String -> Validation String Int
        failure = Failure
        success :: Int -> Validation String Int
        success = Success
    print $ success 1 <> failure "blah"
    print $ failure "woot" <> failure "blah"
    print $ success 1 <> success 2
    print $ failure "woot" <> success 2
    -- Monoid (8)
    putStrLn "\nMonoid -- exercise 8"
    let rmzero = runMem mempty 0
        rmleft = runMem (fun' `mappend` mempty) 0
        rmright = runMem (mempty `mappend` fun') 0
    print $ rmleft
    print $ rmright
    print (rmzero :: (String, Int))
    print $ rmleft == runMem fun' 0
    print $ rmright == runMem fun' 0
