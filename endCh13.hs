-- endCh13.hs
--
-- End of chapter exercises (ch 13)

import Control.Monad (forever)
import System.Exit (exitSuccess)
import Data.Char (toLower, isAlpha)
import System.IO
import EndCh09 (cEncipher, cDecipher)
import EndCh11 (vEncipher, vDecipher)

-- Caesar (ch 09) and Vigenère (ch 11) ciphers have
-- been modified to accept user input.
-- Those changes are exercised here:
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


-- 2) The palindrome function has a forever loop that
--    never exits. Modify the function so it exits
--    successfully after a False result.

-- 3) Modify the palindrome function so it operates
--    on sentences too!
--    (e.g. "Madam I'm Adam" is a palindrome)

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    --case (line1 == reverse line1) of
    let w = [toLower x | x <- line1, isAlpha x]
    case (w == reverse w) of
        True -> putStrLn "It's a palindrome!"
        False -> do
            putStrLn "Nope!"
            exitSuccess


-- 4)
type Name = String
type Age = Integer

data Person = Person Name Age
    deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
    deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right (Person name age)
    | name == ""            = Left NameEmpty
    | not (age > 0)         = Left AgeTooLow
    | otherwise             = Left $ PersonInvalidUnknown $
                                "Name was: " ++ show name ++
                                " Age was: " ++ show age

-- Complete gimmePerson *without* modifying any of the code above.
-- It should do the following:
--  a) Prompt for name and age
--  b) Attempt to construct a Person with the input values
--  c) If successful, print "Yay! Successfully got a person:"
--     followed by the Person value.
--  d) If error, report that an error occurred and print the error.
gimmePerson :: IO ()
--gimmePerson = undefined
gimmePerson = do
    hSetBuffering stdout NoBuffering
    putStr "Enter the person's name: "
    name <- getLine
    putStrLn ""
    putStr "Enter the person' age: "
    age <- readLn :: IO Integer
    case mkPerson name age of
        Right p -> putStrLn ("Yay! Successfully got a person: " ++ show p)
        Left ip -> putStrLn ("An error occurred: " ++ show ip)
