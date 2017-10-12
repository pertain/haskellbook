{-# LANGUAGE FlexibleInstances #-}
-- PART 1
--type Cows = Int
--type Goats = Int

-- PART 2
newtype Cows = Cows Int
    deriving (Eq, Show)

newtype Goats = Goats Int
    deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a
    where
        tooMany :: a -> Bool

instance TooMany Int
    where
        tooMany n = n > 12

instance TooMany Goats
    where
        tooMany (Goats n) = n > 45

instance TooMany Cows
    where
        tooMany (Cows n) = n > 30


{--
-- PART 3
-- This pragma is needed for deriving custom typeclasses
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

instance TooMany Int
    where
        tooMany n = n > 12

newtype Goats = Goats Int
    deriving (Eq, Show, TooMany)
--}

--{-# LANGUAGE FlexibleInstances #-}
-- Logic Goats (1)
--class TooMany a
    --where
        --tooMany :: a -> Bool

newtype GoatsByName = GoatsByName (Int, String)
    deriving (Eq, Show)

instance TooMany GoatsByName
    where
        tooMany (GoatsByName (n, _)) = n > 35

-- Logic Goats (2)
newtype GoatsByField = GoatsByField (Int, Int)
    deriving (Eq, Show)

instance TooMany GoatsByField
    where
        tooMany (GoatsByField (n, m)) = (n + m) > 60

--{--

instance TooMany (Int, String)
    where
        tooMany (n, _) = n > 43

--instance TooMany (Int, Int)
    --where
        --tooMany (n, m) = (n + m) > 43
--}

{--
-- Logic Goats (3) -- STILL UNDER CONSTRUCTION
newtype Goats = Goats ((Num a, TooMany a) => (a, a))
    deriving (Eq, Show)

instance TooMany Goats
    where
        tooMany (Goats (n, m)) = (n + m) > 75
--}
--newtype GoatSum a = GoatSum (a, a)
    --deriving (Eq, Show)

--instance (Num a, TooMany a, Ord a) => TooMany (GoatSum a)
    --where
        --tooMany (GoatSum (n, m)) = (n + m) > 105
{--
instance TooMany (GoatSum (a, a))
    where
        tooMany (GoatSum (n, m)) = (n + m ) > 105
--}

instance (Num a, TooMany a, Ord a) => TooMany (a, a)
    where
        tooMany (n, m) = (n + m) > 55
