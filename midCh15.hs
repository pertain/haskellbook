-- midCh15.hs
--
-- In-Chapter exercises (ch 15)

--import Control.Monad
import Data.Monoid hiding ((<>))
import Data.Semigroup
import Test.QuickCheck


-- Optional Monoid
data Optional a = Nada | Only a
    deriving (Eq, Show)

instance Monoid a => Monoid (Optional a)
    where
        mempty = Nada
        mappend Nada x = x
        mappend x Nada = x
        mappend (Only x) (Only y) = Only (mappend x y)

instance Monoid a => Semigroup (Optional a) where
    (<>) = mappend


-- Madness
type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' e adv noun adj =
    e    <> "! he said " <>
    adv  <> " as he jumped into his car " <>
    noun <> " and drove off with his " <>
    adj  <> " wife."


madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinBetter' e adv noun adj =
    mconcat [ e, "! he said "
            , adv, " as he jumped into his car "
            , noun, " and drove off with his "
            , adj, " wife."
            ]


-- Validating associativity with QuickCheck
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)


-- Testing right and left identity
monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


-- Testing QuickCheck's patience (testing an invalid monoid)
data Bull = Fools | Twoo
    deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary =
        frequency [ (1, return Fools)
                  , (1, return Twoo)]

instance Monoid Bull where
    mempty = Fools
    mappend _ _ = Fools

instance Semigroup Bull where
    (<>) = mappend

type BullMappend = Bull -> Bull -> Bull -> Bool


-- Exercise: Maybe another Monoid
newtype First' a =
    First' {getFirst' :: Optional a}
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
        a <- arbitrary
        frequency [ (3, return $ First' (Only a))
                  , (1, return $ First' Nada)]

instance Monoid (First' a) where
    mempty = First' Nada
    mappend x (First' Nada) = x
    mappend (First' Nada) x = x
    mappend x _ = x

instance Semigroup (First' a) where
    (<>) = mappend

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String
                 -> First' String
                 -> First' String
                 -> Bool

type FstId = First' String -> Bool


main :: IO ()
main = do
    putStrLn "-----------------------------------------"
    putStrLn "Bull"
    quickCheck (monoidAssoc :: BullMappend)
    quickCheck (monoidLeftIdentity :: Bull -> Bool)
    quickCheck (monoidRightIdentity :: Bull -> Bool)
    putStrLn "-----------------------------------------"

    putStrLn "First'"
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)
    putStrLn "-----------------------------------------"
