-- midCh18.hs
--
-- In-chapter exercises (ch 18)

import Control.Monad (join)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- Write bind in terms of fmap and join
--
bind :: Monad m => (a -> m b) -> m a -> m b
--bind = undefined
bind f ma = join (f <$> ma)


-- Implement the Either Monad
--
data Sum a b = First a | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
    pure = Second
    (<*>) (First a) _ = First a
    (<*>) _ (First a) = First a
    (<*>) (Second f) (Second b) = Second (f b)

instance Monad (Sum a) where
    return = pure
    (>>=) (First a) _ = First a
    (>>=) (Second b) f = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = frequency [(1, First <$> arbitrary),
                           (3, Second <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (Sum a b) where
    (=-=) = eq


type ICS = (Int,Char,String)
type S = String


main :: IO ()
main = do
    putStrLn "-----------------------------------------"
    putStrLn "Sum a b"
    quickBatch $ functor (undefined :: Sum S ICS)
    quickBatch $ applicative (undefined :: Sum S ICS)
    quickBatch $ monad (undefined :: Sum S ICS)
    putStrLn "-----------------------------------------"
