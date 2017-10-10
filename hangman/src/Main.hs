module Main where

import Control.Monad (forever)
import Data.Char (toLower, toUpper)
import Data.Maybe (isJust, fromJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        (intersperse ' ' $
            fmap renderPuzzleChar discovered)
        ++ "  Guesses: " ++ guessed

--type WordList = [String]
newtype WordList = WordList [String]
    deriving (Eq, Show)

minLen :: Int
minLen = 5
--minLen = 1

maxLen :: Int
maxLen = 9
--maxLen = 1

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    --return (lines dict)
    return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
    --aw <- allWords
    --return (filter gameLength aw)
    (WordList aw) <- allWords
    return $ WordList (filter gameLength aw)
    where
        gameLength w =
            let l = length (w :: String)
            in l >= minLen && l <= maxLen

randWord :: WordList -> IO String
--randWord wl = do
randWord (WordList wl) = do
    randIndex <- randomRIO (0, (length wl) - 1)
    return (wl !! randIndex)

randWord' :: IO String
randWord' = gameWords >>= randWord

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _) c = elem c w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ cs) c = elem c cs

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar mc
    | mc == Nothing = '_'
    | otherwise     = fromJust mc

fillInChar :: Puzzle -> Char -> Puzzle
fillInChar (Puzzle w soFar cs) c = Puzzle w newSoFar (c : cs)
    where
        zipper g wc gc = if wc == g then Just wc else gc
        newSoFar = zipWith (zipper c) w soFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess: " ++ [guess]
    case (charInWord puzzle guess , alreadyGuessed puzzle guess) of
        (_,True) -> do
            putStrLn "Duplicate Guess!\n"
            return puzzle
        (True,_) -> do
            putStrLn "Yes!\n"
            return (fillInChar puzzle guess)
        (False,_) -> do
            putStrLn "No!\n"
            --return (fillInChar puzzle guess)
            return (fillInChar puzzle (toUpper guess))

gameOver :: Puzzle -> IO ()
gameOver (Puzzle w cur cs) =
    if misses > 5 then
        do
            putStrLn "Five misses. Game over\n"
            putStrLn ("The word was: " ++ w)
            exitSuccess
    else return ()
        where
            filled = length $ filter (isJust) cur
            guessCount = length cs
            misses = guessCount - filled

gameWin :: Puzzle -> IO ()
gameWin (Puzzle w cur _) =
    if all isJust cur then
        do
            putStrLn "You Win!"
            putStrLn ("The word was: " ++ w)
            exitSuccess
    else
        return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn ("Word: " ++ show puzzle)
    putStr "\nGuess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "Guess must be a single character"

main :: IO ()
main = do
    word <- randWord'
    let puzzle = freshPuzzle (fmap toLower word)
    putStrLn ""
    runGame puzzle
