-- endCh12.hs
--
-- End of chapter exercises (ch 12)

import Data.Char (toLower)


-- String Processing exercises
--
-- 1) Write a recursive function named replaceThe which takes a
--    text/string, breaks it into words, and replaces each instance
--    of "the" with "a". It's intended to replace only the word "the".
replaceThe :: String -> String
replaceThe = unwords . swap . words
    where
        swap :: [String] -> [String]
        swap []         = []
        swap ("the":xs) = "a" : swap xs
        swap (x:xs)     = x : swap xs


-- Helper function for countTheBeforeVowel
beginsWithVowel :: String -> Integer
beginsWithVowel []    = 0
beginsWithVowel (x:_) = case elem (toLower x) "aeiouy" of
    True -> 1
    _    -> 0

-- 2) Write a recursive function that takes a text/string,
--    breaks it into words, and counts the number of instances
--    of "the" followed by a vowel-initial word.
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = next . words
    where
        next :: [String] -> Integer
        next []           = 0
        next ("the":x:xs) = beginsWithVowel x + next xs
        next (_:xs)       = next xs


-- 3) Return the number of vowels in a word.
countVowels :: String -> Integer
countVowels [] = 0
countVowels (x:xs) = case elem (toLower x) "aeiouy" of
    True -> 1 + countVowels xs
    _    -> countVowels xs


-- Validate the Word exercises
--
newtype Word' = Word' String
    deriving (Eq, Show)

vowels :: String
vowels = "aeiouy"

mkWord :: String -> Maybe Word'
mkWord s = case cc >= vc of
    True -> Just (Word' s)
    _    -> Nothing
    where
        vc = length $ filter (flip elem vowels) s
        cc = length s - vc


-- It's Only Natural
data Nat = Zero | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat i = case i >= 0 of
    True -> Just (iToN i)
    _    -> Nothing
    where
        iToN :: Integer -> Nat
        iToN 0  = Zero
        iToN i' = Succ (iToN (i' - 1))


-- Small Library for Maybe
--
-- 1) Simple boolean checks for Maybe values
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just _) = False

-- 2) The following is a Maybe catamorphism. You can turn
--    a Maybe value into anything else with this.
mayybe :: b -> (a -> b) -> Maybe a -> b
mayybe b _ Nothing  = b
mayybe _ f (Just a) = f a

-- 3)
fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  = a
fromMaybe _ (Just a) = a

-- 4) Converting between List and Maybe
listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

-- 5) Drop Nothings from list
catMaybes :: [Maybe a] -> [a]
catMaybes []            = []
catMaybes (Nothing:xs)  = catMaybes xs
catMaybes ((Just x):xs) = x : catMaybes xs

-- 6) You'll see this called "sequence" later.
flipMaybe :: Eq a => [Maybe a] -> Maybe [a]
flipMaybe ms = case elem Nothing ms of
    True -> Nothing
    _    -> Just (go ms)
    where
        go :: [Maybe a] -> [a]
        go []            = []
        go ((Just x):xs) = x : go xs


-- Small library for Either
--
-- 1) Extract all Left values from a list of Either
--    Try to eventually arrive at a solution that uses foldr.
lefts' :: [Either a b] -> [a]
lefts' = foldr getLeft []
    where
        getLeft (Left a) acc = a : acc
        getLeft _ acc        = acc

-- 2) Same as the last one. Use foldr eventually.
rights' :: [Either a b] -> [b]
rights' = foldr getRight []
    where
        getRight (Right b) acc = b : acc
        getRight _ acc         = acc

-- 3)
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' e = (lefts' e, rights' e)

-- 4)
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just (f b)
eitherMaybe' _ (Left _)  = Nothing

-- 5) This is a general catamorphism for Either values
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)  = f a
either' _ g (Right b) = g b

-- 6) Same as before, but use the either' function you just wrote
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left _)    = Nothing
eitherMaybe'' f e@(Right b) = Just (either' (\a -> f b) f e)


-- Unfolds
--
-- 1) Write the function myIterate using direct recursion.
--    The output should match that of 'iterate'.
myIterate :: (a -> a) -> a -> [a]
myIterate f x = go f [] x
    where
        go :: (a -> a) -> [a] -> a -> [a]
        go f' xs x' = x' : go f' (f' x' : xs) (f' x')

-- 2) Write the function myUnfoldr using direct recursion.
--    The output should match that of 'unfoldr'.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = go f [] x
    where
        go :: (b -> Maybe (a, b)) -> [a] -> b -> [a]
        go f' ys x' = case f' x' of
            Nothing       -> []
            Just (y, x'') -> y : go f' (y : ys) x''

-- 3) Rewrite myIterate into betterIterate using myUnfoldr.
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x' -> Just (x', f x')) x


-- Unfold for Binary Tree
--
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Show)

-- 1) Write unfold for BinaryTree
unfoldBT :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfoldBT f x = go f Leaf x
    where
        go :: (a -> Maybe (a, b, a)) -> BinaryTree b -> a -> BinaryTree b
        go f' bt x' = case f' x' of
            Nothing          -> Leaf
            Just (xl, y, xr) -> Node (go f' bt xl) y (go f' bt xr)

-- 2) Write treeBuild using unfoldBT
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfoldBT go 0
    where
        go :: Integer -> Maybe (Integer, Integer, Integer)
        go i = case i < n of
            True  -> Just (i + 1, i, i + 1)
            False -> Nothing
