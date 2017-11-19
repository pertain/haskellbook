-- test.hs

import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- This forces the putStr immediately
    putStr "What's your name? "
    name <- getLine
    sayHello name

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!!")

timesThree :: Int -> Int
timesThree = (*3)

sumTimesThree :: Int -> Int -> Int
sumTimesThree x y = (x + y) * 3

xSquaredTimesThreeOneFour :: Int -> Float
xSquaredTimesThreeOneFour x = (fromIntegral x ^ 2) * 3.14

xSquaredTimesPi :: Int -> Float
xSquaredTimesPi x = (fromIntegral x ^ 2) * pi
