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
-- Does it typecheck? If not, then fix it. (ch 06)
-------------------------------------------------------------

--data Person = Person Bool
newtype Person = Person Bool
    deriving Show

printPerson :: Person -> IO ()
--printPerson = putStrLn . show
printPerson = print

------------------------------------------

data Mood = Blah | Woot deriving (Eq,Show)

settleDown x =
    if x == Woot
        then Blah
        else x

------------------------------------------
