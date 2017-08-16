-------------------------------------------------------------
-- Given a type, write the function (ch 05)
-------------------------------------------------------------

myfunc :: (x -> y) -> (y -> z) -> c -> (a,x) -> (a,z)
myfunc xToY yToZ _ (a,x) = (a, (yToZ . xToY) x)

i :: a -> a
i x = x

c :: a -> b -> a
c x _ = x

c' :: a -> b -> b
c' _ y = y

r :: [a] -> [a]
r []        = []
r [x]       = [x]
r (_:xs)    = xs

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB = bToC . aToB

a :: (a -> c) -> a -> a
a _ x = x

a' :: (a -> b) -> a -> b
a' aToB = aToB


-------------------------------------------------------------
-- Does it typecheck? If not, fix it. (ch 06)
-------------------------------------------------------------

--data Person = Person Bool
newtype Person = Person Bool
    deriving Show

printPerson :: Person -> IO ()
--printPerson = putStrLn . show
printPerson = print

---------------------------------------------

data Mood = Blah | Woot
    deriving (Eq, Show)

settleDown x =
    if x == Woot
        then Blah
        else x

---------------------------------------------

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object
    deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

---------------------------------------------

data Rocks = Rocks String
    deriving (Eq, Show)

data Yeah = Yeah Bool
    deriving (Eq, Show)

data Papu = Papu Rocks Yeah
    deriving (Eq, Show)

-- Does not typecheck
--  Papu takes Rocks Yeah, not String Bool
--phew = Papu "chases" True

-- Does typecheck
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- Does typecheck
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

-- Does not typecheck
--  No Ord instance for Papu
--comparePapus :: Papu -> Papu -> Bool
--comparePapus p p' = p > p'
