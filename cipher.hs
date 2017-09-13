module Cipher where

import Data.Char

caesarCipher :: Int -> Char -> Char
caesarCipher n c
    | generalCategory c == LowercaseLetter  = chr (ord 'a' + (mod ((ord c - ord 'a') + n) 26))
    | generalCategory c == UppercaseLetter  = chr (ord 'A' + (mod ((ord c - ord 'A') + n) 26))
    | otherwise                             = '\NUL'

encipher :: Int -> String -> String
encipher n = map (caesarCipher n)

decipher :: Int -> String -> String
decipher n = map (caesarCipher (negate n))
