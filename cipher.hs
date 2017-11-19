-- Caesar Cipher (ch. 09), Vigenère Cipher (ch. 11)

module Cipher where

import System.IO
import Data.Char
import Data.List.Split (splitPlaces)

type ShiftSize = Int
type InputChar = Char
type ShiftedChar = Char
type Key = String
type ExpandedKey = String
type Plain = String
type Encrypted = String

-- Added during end of chapter exercises (ch 13)
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStr "\nChoose a cipher.\n(C)aesar or (V)igenère: "
    ciph <- getChar
    putStrLn "\n"
    putStr "(E)ncode or (D)ecipher: "
    direction <- getChar
    putStrLn ""
    case (toLower ciph == 'c', toLower ciph == 'v') of
        (True, _)      -> do
            putStrLn "Caesar Cipher"
            putStr "Enter the shift size: "
            s <- readLn :: IO Int
            putStrLn ""
            putStr "Enter the word: "
            w <- getLine
            case (toLower direction == 'e', toLower direction == 'd') of
                (True, _)      -> putStrLn (w ++ " -> " ++ cEncipher s w)
                (_, True)      -> putStrLn (w ++ " -> " ++ cDecipher s w)
                (False, False) -> return ()
        (_, True)      -> do
            putStrLn "Vigenère Cipher"
            putStr "Enter the key string: "
            k <- getLine
            putStr "Enter the word: "
            w <- getLine
            case (toLower direction == 'e', toLower direction == 'd') of
                (True, _)      -> putStrLn (w ++ " -> " ++ vEncipher k w)
                (_, True)      -> putStrLn (w ++ " -> " ++ vDecipher k w)
                (False, False) -> return ()
        (False, False) -> do
            putStrLn "Not a valid choice (Enter C or V)"
            return ()
    return ()

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


-- Vigenère Cipher
vigenèreCipher :: (ShiftSize, InputChar) -> ShiftedChar
vigenèreCipher = uncurry caesarCipher

vFullKey :: Key -> String -> ExpandedKey
vFullKey k s = unwords $ splitPlaces ls ks
    where
        ks = take (length s) (concat $ repeat k)
        ls = map length (words s)

vEncipher :: Key -> Plain -> Encrypted
vEncipher k s = map vigenèreCipher pairs
    where
        shifts = map relativeCharIndex (vFullKey k s)
        pairs  = zip shifts s

vDecipher :: Key -> Encrypted -> Plain
vDecipher k s = map vigenèreCipher pairs
    where
        shifts = map (negate . relativeCharIndex) (vFullKey k s)
        pairs  = zip shifts s
