data TisAnInteger =
    TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn a) (TisAn a') = a == a'

---------------------------------------------------------

data TwoIntegers =
    Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two a b) (Two a' b') = a == a' && b == b'

---------------------------------------------------------

data StringOrInt =
    TisAnInt Int | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt a) (TisAnInt a')     = a == a'
    (==) (TisAString b) (TisAString b') = b == b'
    (==) (TisAnInt _) (TisAString _)    = False
    (==) (TisAString _) (TisAnInt _)    = False

---------------------------------------------------------

data Pair a =
    Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair u v) (Pair u' v') = u == u' && v == v'

---------------------------------------------------------

data Tuple a b =
    Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x y) (Tuple x' y')  = x == x' && y == y'

---------------------------------------------------------

data Which a =
    ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
    (==) (ThisOne x) (ThisOne x')   = x == x'
    (==) (ThatOne y) (ThatOne y')   = y == y'
    (==) (ThisOne _) (ThatOne _)    = False
    (==) (ThatOne _) (ThisOne _)    = False

---------------------------------------------------------

data EitherOr a b =
    Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello x')       = x == x'
    (==) (Goodbye y) (Goodbye y')   = y == y'
    (==) (Hello _) (Goodbye _)      = False
    (==) (Goodbye _) (Hello _)      = False
