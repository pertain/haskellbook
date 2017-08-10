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


