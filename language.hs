-- Language exercises (ch 11)

import Data.Char (toUpper)
import Data.List.Split (splitOneOf)

-- 1) Write a function that capitalizes a word.
capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

{-- UNDER CONSTRUCTION
-- 2) Write a function that capitalizes sentences
--    in a paragraph. Recognize when a new
--    sentence has begun by checkin for periods.
--    Reuse the capitalizeWord function.
capitalizeParagraph :: String -> String
capitalizeParagraph s = unwords sentences
    where
        ss = splitOneOf ".?!" s
        sentences = map (capitalizeWord . tail) ss
--}
