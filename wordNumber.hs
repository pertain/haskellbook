-- The is an exercise from the end of Ch 08

module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = case n of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"

digits :: Int -> [Int]
--digits n = (???)
digits 0 = []
digits n = let cur = mod n 10 in
    --cur : digits (div (n - cur) 10)
    digits (div (n - cur) 10) ++ [cur]

wordNumber :: Int -> String
--wordNumber n = (???)
wordNumber n = (concat . intersperse "-")
               (map digitToWord (digits n))
