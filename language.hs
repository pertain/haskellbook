-- Language exercises (ch 11)

import Data.Char (toUpper)
import qualified Data.List.Split as LS

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
