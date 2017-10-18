-- Caesar Cipher (ch. 09)

module Cipher where

import Data.Char

caesarCipher :: Int -> Char -> Char
caesarCipher n c
    | isLC      = chr (ord 'a' + (mod ((ord c - ord 'a') + n) 26))
    | isUC      = chr (ord 'A' + (mod ((ord c - ord 'A') + n) 26))
    | otherwise = ' '
    where
        isLC = generalCategory c == LowercaseLetter
        isUC = generalCategory c == UppercaseLetter

cEncipher :: Int -> String -> String
cEncipher n = map (caesarCipher n)

cDecipher :: Int -> String -> String
cDecipher n = map (caesarCipher (negate n))

--vigenereCipher :: 
