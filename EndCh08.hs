-- endCh08.hs
--
-- End of chapter exercises (ch 08)

module EndCh08 (digitToWord, digits, wordNumber) where

import Data.List (intersperse)


-- Fix dividedBy
--
-- dividedBy is an in-chapter exercise (ch 08) that implements
-- integer division division using recursive subtraction
-- (it bottoms with denominators of 0)

{-
 -- Naive and incomplete
 -- (doesn't handle denominator <= 0)
dividedBy :: Integral a => a -> a -> a
dividedBy numer denom = go numer denom 0
    where
        go n d q
            | n < d     = q
            | otherwise = go (n - d) d (q + 1)
-}

-- Wraps output in Maybe to deal with denom = 0
-- (need to add support for denom < 0)
dividedBy :: Integral a => a -> a -> Maybe a
dividedBy numer denom = go numer denom (Just 0)
    where
        go n d (Just q)
            | d == 0    = Nothing
            | n < d     = Just q
            | otherwise = go (n - d) d (Just (q + 1))


-- McCarthy 91 function
--
mc91 :: Integral a => a -> a
mc91 n
    | n > 100   = n - 10
    | otherwise = mc91 (mc91 (n + 11))


-- Numbers into words
--
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
--digits n = undefined
digits 0 = []
digits n = let cur = mod n 10 in
    --cur : digits (div (n - cur) 10)
    digits (div (n - cur) 10) ++ [cur]

wordNumber :: Int -> String
--wordNumber n = undefined
wordNumber n = (concat . intersperse "-")
               (map digitToWord (digits n))
