-- applicativePractice.hs
--
-- In-Chapter exercises (ch 17)

--import Control.Applicative (liftA2)
import Data.Monoid
import Data.List (elemIndex)


-- Exercises: Lookups
--
-- Use pure, <$>, and <*>
-- to make the expressions typecheck

-- 1)
added :: Maybe Integer
--added = (+3) (lookup 3 $ zip [1,2,3] [4,5,6])
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

-- 2)
y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
--tupled = (,) y z
tupled = (,) <$> y <*> z
--tupled = liftA2 (,) y z

-- 3)
x :: Maybe Int
x = elemIndex 3 [1,2,3,4,5]

y' :: Maybe Int
y' = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
--maxed = max' x y'
maxed = max' <$> x <*> y'
--maxed = liftA2 max' x y'

-- 4)
xs = [1,2,3]
ys = [4,5,6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
--summed = sum $ (,) x' y''
summed = sum <$> ((,) <$> x' <*> y'')


-- Exercise: Identity Instance
--
-- Write an Applicative instance for Identity
newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    --fmap = undefined
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    --pure = undefined
    --(<*>) = undefined
    pure = Identity
    Identity f <*> Identity a = Identity (f a)


-- Exercise: Constant Instance
--
-- Write an Applicative instance for Constant
newtype Constant a b = Constant {getConstant :: a}
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    --fmap = undefined
    fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    --pure = undefined
    --(<*>) = undefined
    pure _ = Constant mempty
    Constant m <*> Constant m' = Constant (m <> m')
