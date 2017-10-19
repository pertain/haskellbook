-- As-Pattern exercises (ch 11)

import Data.Char (toUpper)

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
capitalizedWords :: String -> [(String, String)]
capitalizedWords s = map (wordPair) (words s)
    where
        wordPair w@(x:xs) = (w, (toUpper x) : xs)
