-- Type-Kwon-Do (Ch. 5)
--
-- For each instance of (???)
-- create a function that
-- satisfies the typechecker

-------------------------------------

data Woot
data Blah

f :: Woot -> Blah
f x = undefined

g :: (Blah,Woot) -> (Blah,Blah)
--g = (???)
g (b,w) = (b, f w)

-------------------------------------

f1 :: Int -> String
f1 = undefined

g1 :: String -> Char
g1 = undefined

h1 :: Int -> Char
--h1 = (???)
h1 = g1 . f1

-------------------------------------

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
--e = (???)
e = w . q

-------------------------------------

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

-------------------------------------

munge :: (x -> y) -> (y -> (w,z)) -> x -> w
--munge = (???)
munge xToY yToWZ x = (fst . yToWZ . xToY) x
        
