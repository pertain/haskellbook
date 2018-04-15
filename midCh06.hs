-- midCh06.hs
--
-- In-Chapter exercises (ch 06)


-- Implement Ord and Enum instances for DayOfWeek
--
data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Eq,Show)

instance Ord DayOfWeek where
    compare Mon Mon = EQ
    compare Mon _   = GT
    compare Tue Mon = LT
    compare Tue Tue = EQ
    compare Tue _   = GT
    compare Wed Mon = LT
    compare Wed Tue = LT
    compare Wed Wed = EQ
    compare Wed _   = GT
    compare Thu Fri = GT
    compare Thu Sat = GT
    compare Thu Sun = GT
    compare Thu Thu = EQ
    compare Thu _   = LT
    compare Fri Sat = GT
    compare Fri Sun = GT
    compare Fri Fri = EQ
    compare Fri _   = LT
    compare Sat Sun = GT
    compare Sat Sat = EQ
    compare Sat _   = LT
    compare Sun Sun = EQ
    compare Sun _   = LT

instance Enum DayOfWeek where
    succ Mon        = Tue
    succ Tue        = Wed
    succ Wed        = Thu
    succ Thu        = Fri
    succ Fri        = Sat
    succ Sat        = Sun
    succ Sun        = Mon
    fromEnum Mon    = 1
    fromEnum Tue    = 2
    fromEnum Wed    = 3
    fromEnum Thu    = 4
    fromEnum Fri    = 5
    fromEnum Sat    = 6
    fromEnum Sun    = 7
    toEnum 1        = Mon
    toEnum 2        = Tue
    toEnum 3        = Wed
    toEnum 4        = Thu
    toEnum 5        = Fri
    toEnum 6        = Sat
    toEnum 7        = Sun
    toEnum _        = error "A week has 7 days (1-7 are valid)"

data Date = Date DayOfWeek Int
    deriving Show


-- Exercises: Eq Instances
--
-- Write the Eq instance for the datatype provided

-- 1)
data TisAnInteger =
    TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn a) (TisAn a') = a == a'

-- 2)
data TwoIntegers =
    Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two a b) (Two a' b') = a == a' && b == b'

-- 3)
data StringOrInt =
    TisAnInt Int | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt a) (TisAnInt a')     = a == a'
    (==) (TisAString b) (TisAString b') = b == b'
    (==) (TisAnInt _) (TisAString _)    = False
    (==) (TisAString _) (TisAnInt _)    = False

-- 4)
data Pair a =
    Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair u v) (Pair u' v') = u == u' && v == v'

-- 5)
data Tuple a b =
    Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x y) (Tuple x' y')  = x == x' && y == y'

-- 6)
data Which a =
    ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
    (==) (ThisOne x) (ThisOne x')   = x == x'
    (==) (ThatOne y) (ThatOne y')   = y == y'
    (==) (ThisOne _) (ThatOne _)    = False
    (==) (ThatOne _) (ThisOne _)    = False

-- 7)
data EitherOr a b =
    Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello x')       = x == x'
    (==) (Goodbye y) (Goodbye y')   = y == y'
    (==) (Hello _) (Goodbye _)      = False
    (==) (Goodbye _) (Hello _)      = False
