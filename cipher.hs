-- Caesar Cipher (ch. 09), Vigenere Cipher (ch. 11)

module Cipher where

import Data.Char
import Data.List.Split (splitPlaces)

-- Caesar Cipher
caesarCipher :: Int -> Char -> Char
caesarCipher n c
    | isLC      = chr (ord 'a' + (mod (alphIndex + n) 26))
    | isUC      = chr (ord 'A' + (mod (alphIndex + n) 26))
    | otherwise = ' '
    where
        alphIndex = ord (toLower c) - ord 'a'
        isLC = generalCategory c == LowercaseLetter
        isUC = generalCategory c == UppercaseLetter

cEncipher :: Int -> String -> String
cEncipher n = map (caesarCipher n)

cDecipher :: Int -> String -> String
cDecipher n = map (caesarCipher (negate n))


-- Viginere Cipher
vigenereCipher :: (Int, Char) -> Char
vigenereCipher (k, c)
    | vValidChar c = chr (ord 'a' + (mod (alphIndex + k) 26))
    | otherwise = ' '
    where
        alphIndex = ord (toLower c) - ord 'a'

vCipherKey :: String -> String -> String
vCipherKey s k = unwords $ splitPlaces ls ks
    where
        ss = words s
        ks = take (length s) $ concat $ repeat k
        ls = map length ss

vValidChar :: Char -> Bool
vValidChar c = generalCategory (toLower c) == LowercaseLetter

vShiftVal :: Char -> Int
vShiftVal k = ord (toLower k) - ord 'a'

vEncipher :: String -> String -> String
vEncipher s k = map vigenereCipher pairs
    where
        key = vCipherKey s k
        shifts = map vShiftVal key
        pairs = zip shifts s

vDecipher :: String -> String -> String
vDecipher s k = map vigenereCipher pairs
    where
        key = vCipherKey s k
        shifts = map (negate . vShiftVal) key
        pairs = zip shifts s
