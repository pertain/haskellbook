-- endCh05.hs
--
-- End of chapter exercises (ch 05)

{-# LANGUAGE NoMonomorphismRestriction #-}


-- Determine the type
--
-- Each function below omits type declarations
-- and force the compiler to infer.
-- The NoMonomorphism pragma forces each type to
-- be as polymorphic as possible.


-- simple example
-- Without the NoMonomorphismRestriction pragma
-- GHC would infer the type 'Integer'.
-- However, the most polymorphic type that this
-- function can have is 'Num t'.
example = 1


-- 1)
-- All function applications return a value.
-- Determine the value returned by these function
-- applications and the type of that value.
--f1 :: Num a -> a
f1 = (*9) 6

--f2 :: Num a => (a,String)
f2 = head [(0,"doge"),(1,"kitteh")]

--f3 :: (Integer,Char)
f3 = head [(0 :: Integer,"doge"),(1,"kitteh")]

--f4 :: Bool
f4 = if False then True else False

--f5 :: Int
f5 = length [1,2,3,4,5]

--f6 :: Bool
f6 = (length [1,2,3,4]) > (length "TACOCAT")


----------------------------------
-- Given:
x = 5
y = x + 5

-- 2)
-- What is the type of w?
--w :: Num a => a
w = y * 10

-- 3)
-- What is the type of z?
--z :: Num a => a -> a
z y = y * 10

-- 4)
-- What is the type of ff?
--ff :: Fractional a => a
ff = 4 / y

-- 5)   What is the type of fff?
x2  = "Julie"
y2  = " <3 "
z2  = "Haskell"

--fff :: String
fff = x2 ++ y2 ++ z2


-- Given a type, write the function
--
myFunc :: (x -> y) -> (y -> z) -> c -> (a,x) -> (a,z)
--myFunc = undefined
myFunc xToY yToZ _ (a,x) = (a, (yToZ . xToY) x)

i :: a -> a
--i = undefined
i x = x

c :: a -> b -> a
--c = undefined
c x _ = x

c' :: a -> b -> b
--c' = undefined
c' _ y = y

r :: [a] -> [a]
--r = undefined
r []        = []
r [x]       = [x]
r (_:xs)    = xs

co :: (b -> c) -> (a -> b) -> a -> c
--co = undefined
co bToC aToB = bToC . aToB

a :: (a -> c) -> a -> a
--a = undefined
a _ x = x

a' :: (a -> b) -> a -> b
--a' = undefined
a' aToB = aToB


-- Fix it
--
fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

--sndString :: [Char] -> Char
sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing :: [Char]
--sing = if (x > y) then fstString x else sndString y
sing = if (x < y) then fstString x else sndString y
    where
        x = "Singin"
        y = "Somewhere"


-- Type-Kwon-Do
--
-- For each instance of (???)
-- create a function that
-- satisfies the typechecker

-- 1)
data Woot
data Blah

f :: Woot -> Blah
f x = undefined

g :: (Blah,Woot) -> (Blah,Blah)
--g = (???)
g (b,wt) = (b, f wt)


-- 2)
f1' :: Int -> String
f1' = undefined

g1 :: String -> Char
g1 = undefined

h1 :: Int -> Char
--h1 = (???)
h1 = g1 . f1'

-- 3)
data A
data B
data C

q :: A -> B
q = undefined

w' :: B -> C
w' = undefined

e :: A -> C
--e = (???)
e = w' . q

-- 4)
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X,Y) -> (Z,Z)
--xform = (???)
xform (x,y) = (xz x, yz y)

-- 5)
munge :: (x -> y) -> (y -> (u,z)) -> x -> u
--munge = (???)
munge xToY yToWZ x = (fst . yToWZ . xToY) x
