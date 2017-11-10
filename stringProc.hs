-- String Processing exercises -- end of chapter (ch 12)

import Data.Char (toLower)

-- 1) Write a recursive function named replaceThe which takes a
--    text/string, breaks it into words, and replaces each instance
--    of "the" with "a". It's intended to replace only the word "the".
replaceThe :: String -> String
replaceThe = unwords . swap . words
    where
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
        iToN 0  = Zero
        iToN i' = Succ (iToN (i' - 1))
