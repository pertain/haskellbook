-- EndCh11.hs
--
-- End of chapter exercises (ch 11)

module EndCh11 (vEncipher, vDecipher) where

import Data.Char (toUpper, toLower)
import Data.List (elemIndex, group, sort, sortBy)
import Data.Function (on)
import EndCh09 (caesarCipher, relativeCharIndex)
import qualified Data.List.Split as LS


-- Vigenère Cipher
--
type ShiftSize = Int
type InputChar = Char
type ShiftedChar = Char
type Key = String
type ExpandedKey = String
type Plain = String
type Encrypted = String

vigenèreCipher :: (ShiftSize, InputChar) -> ShiftedChar
vigenèreCipher = uncurry caesarCipher

vFullKey :: Key -> String -> ExpandedKey
vFullKey k s = unwords $ LS.splitPlaces ls ks
    where
        ks = take (length s) (concat $ repeat k)
        ls = map length (words s)

vEncipher :: Key -> Plain -> Encrypted
vEncipher k s = map vigenèreCipher pairs
    where
        shifts = map relativeCharIndex (vFullKey k s)
        pairs  = zip shifts s

vDecipher :: Key -> Encrypted -> Plain
vDecipher k s = map vigenèreCipher pairs
    where
        shifts = map (negate . relativeCharIndex) (vFullKey k s)
        pairs  = zip shifts s


-- As-Patterns
--
-- 1) This should return True if (and only if) all the
--    values in the first list appear in the second list,
--    though they need not be contiguous.
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf l1 l2 = l1 == subSeq l1 l2
    where
        subSeq [] _ = []
        subSeq _ [] = []
        subSeq l@(x:xs) (y:ys)
            | x == y = y : subSeq xs ys
            | otherwise = subSeq l ys

-- 2) Split a sentence into words, then tuple each word
--    with the capitalized form of each.
capitalizeWords :: String -> [(String, String)]
capitalizeWords s = map (wordPair) (words s)
    where
        wordPair w@(x:xs) = (w, toUpper x : xs)


-- Language exercises
--
-- 1) Write a function that capitalizes a word.
capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

-- 2) Write a function that capitalizes sentences
--    in a paragraph. Recognize when a new
--    sentence has begun by checkin for periods.
--    Reuse the capitalizeWord function.
capitalizeParagraph :: String -> String
capitalizeParagraph s = unwords sentences
    where
        ss = LS.split (LS.condense $ LS.endsWithOneOf ".!?") s
        capSentence (' ':xs) = capitalizeWord xs
        capSentence s' = capitalizeWord s'
        sentences = map capSentence ss


-- Phone exercise
--
type Digit = Char
type Values = [Char]
type Presses = Int

data Button = Button Digit Values
    deriving (Eq, Show)

data DaPhone = DaPhone [Button]
    deriving (Eq, Show)

phone :: DaPhone
phone = DaPhone [ Button '1' "1"
                , Button '2' "abc2"
                , Button '3' "def3"
                , Button '4' "ghi4"
                , Button '5' "jkl5"
                , Button '6' "mno6"
                , Button '7' "pqrs7"
                , Button '8' "tuv8"
                , Button '9' "wxyz9"
                , Button '0' "+ 0"
                , Button '#' ".,#"
                , Button '*' ""
                ]

convo :: [String]
convo = [ "Wanna play 20 questions"
        , "Ya"
        , "U 1st haha"
        , "Lol ok. Have u ever tasted alcohol"
        , "Lol ya"
        , "Wow ur cool haha. Ur turn"
        , "Ok. Do u think I am pretty Lol"
        , "Lol ya"
        , "Just making sure rofl ur turn"
        ]

-- sorts and groups like-elements
-- e.g. "tacocat" -> ["aa","cc","tt","o"]
sortedGroups :: Ord a => [a] -> [[a]]
sortedGroups = (sortBy ((flip compare) `on` length)) . group . sort

-- Assuming the default phone configuration
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone bs) c = concatMap findButton bs
    where
        charIndex c' cs = case elemIndex c' cs of
            Just i  -> i + 1
            Nothing -> 0
        findButton (Button d vs)
            | elem c vs           = [(d, charIndex c vs)]
            | elem (toLower c) vs = ('*', 1) : [(d, charIndex (toLower c) vs)]
            | otherwise           = []

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead ph s = concatMap (reverseTaps ph) s

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps []         = 0
fingerTaps ((_,p):xs) = p + fingerTaps xs

-- This type signature matches the one provided in the book
mostPopularLetter :: String -> Char
mostPopularLetter s = (head . head . sortedGroups) s

mostPopularLetterCost :: DaPhone -> String -> Presses
mostPopularLetterCost ph s = appearances * letterCost
    where
        letter      = mostPopularLetter s
        letterCost  = fingerTaps (reverseTaps ph letter)
        appearances = length (filter (== letter) s)

-- This is an alternative approach that shifts letter quantity
-- functionality from mostPopularLetterCount to mostPopularLetter
mostPopularLetter' :: String -> (Char, Int)
mostPopularLetter' s = (head . (map pair) . sortedGroups) s
    where
        pair x = (head x, length x)

mostPopularLetterCost' :: DaPhone -> String -> Presses
mostPopularLetterCost' ph s = n * letterCost
    where
        (c, n)     = mostPopularLetter' s
        letterCost = fingerTaps (reverseTaps ph c)

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord ss = (head . head) sorted
    where
        sorted = (sortedGroups . concat . (map words)) ss


-- Hutton's Razor
--
data Expr = Lit Integer | Add Expr Expr

-- Write the "eval" function which reduces an
-- expression to a final sum
eval :: Expr -> Integer
--eval = undefined
eval (Lit x) = x
eval (Add a b) = eval a + eval b

-- Write a printer for the expressions
printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b
