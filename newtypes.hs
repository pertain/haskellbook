{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- pragma needed for deriving custom typeclasses
{-# LANGUAGE FlexibleInstances #-}

-- Newtype exercises (ch 11)


-- PART 1
-- Type aliases -> not as rigid as newtypes
-- functions using the alias type can also use the aliased type
-- (i.e. tooWeird works fine with any Int, including Catdog)
type Blowfish = Int
type Catdog = Int

tooWeird :: Blowfish -> Bool
tooWeird b = (2 * b) == 9


-- PART 2
-- functions using newtype will not work with the underlying type
-- (i.e. tooManyGoats takes Goats Int, but not Int)
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

instance TooMany Double
    where
        tooMany n = n > 13.0

instance TooMany Goats
    where
        tooMany (Goats n) = n > 15


-- PART 3
-- TooMany is derived without the need for an explicit instance
-- (requires the GeneralizedNewtypeDeriving pragma)
newtype DerivedGoats = DerivedGoats Int
    deriving (Eq, Show, TooMany)


-- Logic Goats (1)

-- This overloads class TooMany a (requires the FlexibleInstances pragma)
instance TooMany (Int, String)
    where
        tooMany (n, _) = n > 35

-- This approach sidesteps the need for the FlexibleInstances pragma
newtype GoatsByName = GoatsByName (Int, String)
    deriving (Eq, Show)

instance TooMany GoatsByName
    where
        tooMany (GoatsByName (n, _)) = n > 35


-- Logic Goats (2)

-- This overloads class TooMany a (requires the FlexibleInstances pragma)
-- Commented out because it overlaps with instances in Logic Goats (3)
--instance TooMany (Int, Int)
    --where
        --tooMany (n, m) = (n + m) > 60

-- This approach sidesteps the need for the FlexibleInstances pragma
newtype GoatsByLocation = GoatsByLocation (Int, Int)
    deriving (Eq, Show)

instance TooMany GoatsByLocation
    where
        tooMany (GoatsByLocation (n, m)) = (n + m) > 90


-- Logic Goats (3) -- STILL UNDER CONSTRUCTION

-- This overloads class TooMany a (requires the FlexibleInstances pragma)
-- Works with any tuple (a, a) of types (Ord a, Num a)
-- but requires explicit type declarations.
-- This is more generic than the next instance
--instance (Ord a, Num a) => TooMany (a, a)
    --where
        --tooMany (n, m) = (n + m) > 48

-- This overloads class TooMany a (requires the FlexibleInstances pragma)
-- Works with any tuple (a, a) of types (Ord a, Num a, TooMany a).
-- The thing to note here is that it only works for defined instances
-- of TooMany a (i.e. TooMany Int, or TooMany Double)
-- This is less generic than the previous instance
instance (Ord a, Num a, TooMany a) => TooMany (a, a)
    where
        tooMany (n, m) = (n + m) > 49

-- This approach sidesteps the need for the FlexibleInstances pragma
newtype WayTooMany a = WayTooMany a
    deriving (Eq, Show)

instance (Ord a, Num a) => TooMany (WayTooMany (a, a))
    where
        tooMany (WayTooMany (n, m)) = (n + m) > 49
