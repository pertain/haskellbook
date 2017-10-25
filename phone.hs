module Phone where

import Data.List (elemIndex)
import Data.Char (toLower)

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

-- Assuming the default phone configuration
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone bs) c = concatMap findButton bs
    where
        charIndex c' cs = case elemIndex c' cs of
            Just i  -> i + 1
            Nothing -> 0
        findButton bt@(Button d vs)
            | elem c vs           = [(d, charIndex c vs)]
            | elem (toLower c) vs = ('*', 1) : [(d, charIndex (toLower c) vs)]
            | otherwise           = []
