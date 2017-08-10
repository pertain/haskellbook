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
