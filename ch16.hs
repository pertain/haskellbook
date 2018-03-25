-- ch16.hs
--
-- End of chapter exercises (ch 16 - Functor)

{-# LANGUAGE FlexibleInstances #-} -- Used for 3.3

import Control.Monad (liftM, liftM2, liftM3, liftM4)
import Test.QuickCheck
import Test.QuickCheck.Function
import GHC.Arr  -- Used for 1.5

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
-- Determine if a valid Functor can be
-- written for the datatype provided

-- 1.1) No [kind: *]
data Boolean =
    False | True

-- 1.2) Yes [kind: * -> *]
data BoolAndSomethingElse a =
    False' a | True' a
    deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
    fmap f (False' a) = False' (f a)
    fmap f (True' a) = True' (f a)

instance Arbitrary a => Arbitrary (BoolAndSomethingElse a) where
    arbitrary = do
        oneof [(liftM False' arbitrary),
               (liftM True' arbitrary)]

type BoolAndSomethingElseIdentity = BoolAndSomethingElse Int
                                 -> Bool

type BoolAndSomethingElseCompose = BoolAndSomethingElse Char
                                -> Fun Char Int
                                -> Fun Int Char
                                -> Bool

-- 1.3) Yes [kind: * -> *]
data BoolAndMaybeSomethingElse a =
    Falsish | Truish a
    deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
    fmap _ Falsish = Falsish
    fmap f (Truish a) = Truish (f a)

instance Arbitrary a => Arbitrary (BoolAndMaybeSomethingElse a) where
    arbitrary = do
        frequency [(1, return $ Falsish),
                   (3, liftM Truish arbitrary)]

type BoolAndMaybeSomethingElseIdentity = BoolAndMaybeSomethingElse Int
                                      -> Bool

type BoolAndMaybeSomethingElseCompose = BoolAndMaybeSomethingElse Char
                                     -> Fun Char (Maybe Int)
                                     -> Fun (Maybe Int) Char
                                     -> Bool


-- 1.4) No [kind: (* -> *) -> *]
--      Even if we make f a Functor, we cannot make Mu a Functor,
--      because we cannot partially apply f. And, a fully
--      applied f () will result in a fully applied Mu
newtype Mu f = InF { outF :: f (Mu f) }


-- 1.5) No [kind: *]
data D = D (Array Word Word) Int Int



-- 2)
-- Rearrange the arguments to the type constructor
-- of the datatype so the Functor instance works.

-- 2.1)
--data Sum a b = First a | Second b
data Sum b a = First a | Second b
    deriving (Eq, Show)

instance Functor (Sum e) where
    fmap f (First a) = First (f a)
    fmap f (Second b) = Second b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum b a) where
    arbitrary = do
        oneof [(liftM First arbitrary),
               (liftM Second arbitrary)]

type SumIdentity = Sum Char Int
                -> Bool

type SumCompose = Sum Int Char
               -> Fun Char Bool
               -> Fun Bool Char
               -> Bool

-- 2.2)
--data Company a b c = DeepBlue a c | Something b
data Company a c b = DeepBlue a c | Something b
    deriving (Eq, Show)

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c

instance (Arbitrary a, Arbitrary b, Arbitrary c)
    => Arbitrary (Company a c b) where
    arbitrary = do
        frequency [(1, liftM2 DeepBlue arbitrary arbitrary),
                   (3, liftM Something arbitrary)]

type CompanyIdentity = Company (Maybe Int) Char Int
                    -> Bool

type CompanyCompose = Company Bool Char Int
                   -> Fun Int (Maybe Char)
                   -> Fun (Maybe Char) Int
                   -> Bool

-- 2.3)
--data More a b = L a b a | R b a b
data More b a = L a b a | R b a b
    deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a ) b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (More b a) where
    arbitrary = do
        oneof [(liftM3 L arbitrary arbitrary arbitrary),
               (liftM3 R arbitrary arbitrary arbitrary)]

type MoreIdentity = More (Either Int Char) Int
                 -> Bool

type MoreCompose = More Int (Maybe Bool)
                -> Fun (Maybe Bool) (Maybe Int)
                -> Fun (Maybe Int) (Maybe Bool)
                -> Bool


-- 3)
-- Write the Functor instances for the following datatypes

-- 3.1)
data Quant a b = Finance | Desk a | Bloor b
    deriving (Eq, Show)

instance Functor (Quant x) where
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
    arbitrary = do
        frequency [(1, return Finance),
                   (2, liftM Desk arbitrary),
                   (4, liftM Bloor arbitrary)]

type QuantIdentity = Quant Char Int
                  -> Bool

type QuantCompose = Quant Bool (Either Char Char)
                 -> Fun (Either Char Char) Int
                 -> Fun Int (Either Char Char)
                 -> Bool

{--
-- 3.2)
data K a b = K a
    deriving (Eq, Show)

instance Functor (K x) where
    fmap _ (K a) = K a

instance (Arbitrary a, Arbitrary b) => Arbitrary (K a b) where
    arbitrary = liftM K arbitrary

type KIdentity = K String Int
              -> Bool

type KCompose = K Int Char
             -> Fun Char (Maybe Char)
             -> Fun (Maybe Char) Char
             -> Bool
--}

-- 3.3)
newtype Flip f a b = Flip (f b a)
    deriving (Eq, Show)

newtype K a b = K a
    deriving (Eq, Show)

instance Functor (Flip K a) where
    fmap f (Flip (K a)) = Flip (K (f a))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Flip K a b) where
    --arbitrary = undefined
    arbitrary = do
        a <- arbitrary
        return (Flip (K a))

type FlipKIdentity = Flip K Int Char
                  -> Bool

type FlipKCompose = Flip K Bool Int
                 -> Fun Int (Maybe Char)
                 -> Fun (Maybe Char) Int
                 -> Bool

-- 3.4)
data EvilGoateeConst a b = GoatyConst b
    deriving (Eq, Show)

instance Functor (Flip EvilGoateeConst x) where
    fmap _ (Flip (GoatyConst b)) = Flip (GoatyConst b)

instance (Arbitrary a, Arbitrary b)
    => Arbitrary (Flip EvilGoateeConst a b) where
    arbitrary = do
        b <- arbitrary
        return (Flip (GoatyConst b))

type FlipEvilGoateeConstIdentity = Flip EvilGoateeConst Int String
                                -> Bool

type FlipEvilGoateeConstCompose = Flip EvilGoateeConst Char Int
                               -> Fun Int (Either Int Char)
                               -> Fun (Either Int Char) Int
                               -> Bool

-- 3.5)
data LiftItOut f a = LiftItOut (f a)
    deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

instance Arbitrary a => Arbitrary (LiftItOut Maybe a) where
    arbitrary = do
        frequency [(1, return $ LiftItOut Nothing),
                   (3, liftM LiftItOut (liftM Just arbitrary))]

type LiftItOutIdentity = LiftItOut Maybe String
                      -> Bool

type LiftItOutCompose = LiftItOut Maybe Char
                     -> Fun Char Int
                     -> Fun Int Char
                     -> Bool

-- 3.6)
data Parappa f g a = DaWrappa (f a) (g a)
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa fa') = DaWrappa (fmap f fa) (fmap f fa')

instance Arbitrary a => Arbitrary (Parappa Maybe [] a) where
    arbitrary = do
        a <- arbitrary
        frequency [(1, return $ DaWrappa Nothing [a]),
                   (3, return $ DaWrappa (Just a) [a])]

type ParappaIdentity = Parappa Maybe [] Char
                    -> Bool

type ParappaCompose = Parappa Maybe [] Char
                   -> Fun Char (Int,Char)
                   -> Fun (Int,Char) Char
                   -> Bool

-- 3.7)
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
    deriving (Eq, Show)

instance (Functor f, Functor g)
    => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb)
        = IgnoringSomething fa (fmap f gb)

instance (Arbitrary a, Arbitrary b)
    => Arbitrary (IgnoreOne [] Maybe a b) where
    arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ IgnoringSomething [a] Nothing),
               (3, return $ IgnoringSomething [a] (Just b))]

type IgnoreOneIdentity = IgnoreOne [] Maybe Int (Maybe String)
                      -> Bool

type IgnoreOneCompose = IgnoreOne [] Maybe Int (Either Int Char)
                     -> Fun (Either Int Char) (Maybe Char)
                     -> Fun (Maybe Char) (Either Int Char)
                     -> Bool

-- 3.8)
data Notorious g o a t = Notorious (g o) (g a) (g t)
    deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

instance (Arbitrary o, Arbitrary a, Arbitrary t)
    => Arbitrary (Notorious [] o a t) where
    arbitrary = do
    o <- arbitrary
    a <- arbitrary
    t <- arbitrary
    return $ Notorious [o] [a] [t]

type NotoriousIdentity = Notorious [] String Int Char
                      -> Bool

type NotoriousCompose = Notorious [] Int Int (Maybe Char)
                     -> Fun (Maybe Char) Char
                     -> Fun Char (Maybe Char)
                     -> Bool


runQc :: IO ()
runQc = do
    putStrLn "--------------------------------"
    putStrLn "1.2) BoolAndSomethingElse a"
    quickCheck (functorIdentity :: BoolAndSomethingElseIdentity)
    --verboseCheck (functorCompose' :: BoolAndSomethingElseCompose)
    quickCheck (functorCompose' :: BoolAndSomethingElseCompose)
    putStrLn "1.3) BoolAndMaybeSomethingElse a"
    quickCheck (functorIdentity :: BoolAndMaybeSomethingElseIdentity)
    quickCheck (functorCompose' :: BoolAndMaybeSomethingElseCompose)
    putStrLn "--------------------------------"
    putStrLn "2.1) Sum b a"
    quickCheck (functorIdentity :: SumIdentity)
    quickCheck (functorCompose' :: SumCompose)
    putStrLn "2.2) Company a c b"
    quickCheck (functorIdentity :: CompanyIdentity)
    quickCheck (functorCompose' :: CompanyCompose)
    putStrLn "2.3) More b a"
    quickCheck (functorIdentity :: MoreIdentity)
    quickCheck (functorCompose' :: MoreCompose)
    putStrLn "--------------------------------"
    putStrLn "3.1) Quant a b"
    quickCheck (functorIdentity :: QuantIdentity)
    quickCheck (functorCompose' :: QuantCompose)
    --putStrLn "3.2) K a b"
    --quickCheck (functorIdentity :: KIdentity)
    --quickCheck (functorCompose' :: KCompose)
    putStrLn "3.3) Flip K a b"
    quickCheck (functorIdentity :: FlipKIdentity)
    quickCheck (functorCompose' :: FlipKCompose)
    putStrLn "3.4) Flip EvilGoateeConst a b"
    quickCheck (functorIdentity :: FlipEvilGoateeConstIdentity)
    quickCheck (functorCompose' :: FlipEvilGoateeConstCompose)
    putStrLn "3.5) LiftItOut f a"
    quickCheck (functorIdentity :: LiftItOutIdentity)
    quickCheck (functorCompose' :: LiftItOutCompose)
    putStrLn "3.6) Parappa f g a"
    quickCheck (functorIdentity :: ParappaIdentity)
    quickCheck (functorCompose' :: ParappaCompose)
    putStrLn "3.7) IgnoreOne f g a b"
    quickCheck (functorIdentity :: IgnoreOneIdentity)
    quickCheck (functorCompose' :: IgnoreOneCompose)
    putStrLn "3.8) Notorious g o a t"
    quickCheck (functorIdentity :: NotoriousIdentity)
    quickCheck (functorCompose' :: NotoriousCompose)
    putStrLn "--------------------------------"
