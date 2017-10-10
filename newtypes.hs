--type Cows = Int
--type Goats = Int
newtype Cows = Cows Int
    deriving (Eq, Show)

newtype Goats = Goats Int
    deriving (Eq, Show)

--tooManyGoats :: Int -> Bool
tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a
    where
        tooMany :: a -> Bool

instance TooMany Int
    where
        tooMany n = n > 42

instance TooMany Goats
    where
        tooMany (Goats n) = n > 42
