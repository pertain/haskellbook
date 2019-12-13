-- endCh22.hs
--
-- End of chapter exercises (ch 22)


import Control.Applicative
import Data.Maybe

x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

-- Write each function given the type signature

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- create lookup that returns Nothing
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- now zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

-- make a tuple of xs and ys
x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

-- make a tuple of ys and zs
x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

-- make a tuple of z' and z'
x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

-- sum values in a Maybe tuple
summed :: Num c => (c, c) -> c
summed = uncurry (+)

-- lift a boolean function over
-- two partially applied functions
--
-- use &&, >3, and <8
bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
    print $ sequenceA [Just 3, Just 2, Just 1]
    print $ sequenceA [x, y]
    print $ sequenceA [xs, ys]
    print $ summed <$> ((,) <$> xs <*> ys)
    print $ fmap summed ((,) <$> xs <*> zs)
    print $ bolt 7
    print $ fmap bolt z
    ----------------------------------------
    -- 1) fold the boolean conjunction operator
    --    over the list of results of sequA
    print $ foldr (&&) True (sequA 7)
    -- 2) apply sequA to s'
    print $ sequA $ fromMaybe 0 s'
    -- 3) apply bolt to ys
    print $ bolt $ fromMaybe 0 ys
