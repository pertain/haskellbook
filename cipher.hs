-- Caesar Cipher (ch. 09), Vigenere Cipher (ch. 11)

module Cipher where

import Data.Char
import Data.List.Split (splitPlaces)

type ShiftSize = Int
type InputChar = Char
type ShiftedChar = Char
type Key = String
type ExpandedKey = String
type Plain = String
type Encrypted = String

-- Common Functions
relativeCharIndex :: Char -> Int
relativeCharIndex k = ord (toLower k) - ord 'a'


-- Caesar Cipher
caesarCipher :: ShiftSize -> InputChar -> ShiftedChar
caesarCipher n c = case generalCategory c of
    LowercaseLetter -> chr (ord 'a' + mod (relativeCharIndex c + n) 26)
    UppercaseLetter -> chr (ord 'A' + mod (relativeCharIndex c + n) 26)
    _               -> ' '

cEncipher :: ShiftSize -> Plain -> Encrypted
cEncipher n = map (caesarCipher n)

cDecipher :: ShiftSize -> Encrypted -> Plain
cDecipher n = map (caesarCipher (negate n))


-- Viginere Cipher
vigenereCipher :: (ShiftSize, InputChar) -> ShiftedChar
vigenereCipher = uncurry caesarCipher

vFullKey :: Key -> String -> ExpandedKey
vFullKey k s = unwords $ splitPlaces ls ks
    where
        ks = take (length s) (concat $ repeat k)
        ls = map length (words s)

vEncipher :: Key -> Plain -> Encrypted
vEncipher k s = map vigenereCipher pairs
    where
        shifts = map relativeCharIndex (vFullKey k s)
        pairs  = zip shifts s

vDecipher :: Key -> Encrypted -> Plain
vDecipher k s = map vigenereCipher pairs
    where
        shifts = map (negate . relativeCharIndex) (vFullKey k s)
        pairs  = zip shifts s
