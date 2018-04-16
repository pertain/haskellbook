-- EndCh09.hs
--
-- End of chapter exercises (ch 09)

module EndCh09 (caesarCipher,
                relativeCharIndex,
                cEncipher,
                cDecipher) where

import Data.Char


-- Caesar Cipher
type ShiftSize = Int
type InputChar = Char
type ShiftedChar = Char
type Key = String
type ExpandedKey = String
type Plain = String
type Encrypted = String


-- Caesar Cipher
--
caesarCipher :: ShiftSize -> InputChar -> ShiftedChar
caesarCipher n c = case generalCategory c of
    LowercaseLetter -> chr (ord 'a' + mod (relativeCharIndex c + n) 26)
    UppercaseLetter -> chr (ord 'A' + mod (relativeCharIndex c + n) 26)
    _               -> ' '

relativeCharIndex :: Char -> Int
relativeCharIndex k = ord (toLower k) - ord 'a'

cEncipher :: ShiftSize -> Plain -> Encrypted
cEncipher n = map (caesarCipher n)

cDecipher :: ShiftSize -> Encrypted -> Plain
cDecipher n = map (caesarCipher (negate n))
