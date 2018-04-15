-- endCh06.hs
--
-- End of chapter exercises (ch 06)


-- Does it typecheck? If not, fix it.
--

-- 1)
--data Person = Person Bool
newtype Person = Person Bool
    deriving Show

printPerson :: Person -> IO ()
--printPerson = putStrLn . show
printPerson = print

-- 2)
data Mood = Blah | Woot
    deriving (Eq, Show)

settleDown x =
    if x == Woot
        then Blah
        else x

-- 3)
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object
    deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

-- 4)
data Rocks = Rocks String
    deriving (Eq, Show)

data Yeah = Yeah Bool
    deriving (Eq, Show)

data Papu = Papu Rocks Yeah
    deriving (Eq, Show)

-- 5)
-- Does not typecheck
--  Papu takes Rocks Yeah, not String Bool
--phew = Papu "chases" True

-- 6)
-- Does typecheck
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- 7)
-- Does typecheck
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

-- 8)
-- Does not typecheck
--  No Ord instance for Papu
--comparePapus :: Papu -> Papu -> Bool
--comparePapus p p' = p > p'


-- Type-Kwon-Do Two: Electric Typealoo
--
chk :: Eq b => (a -> b) -> a -> b -> Bool
--chk = (???)
chk aToB a b = (aToB a) == b

arith :: Num b => (a -> b) -> Integer -> a -> b
--arith = (???)
arith aToB x a = aToB a
