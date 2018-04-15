-- midCh09.hs
--
-- In-Chapter exercises (ch 09)


-- Exercises: Thy Fearful Symmetry
--
hns         = "holas nolas scrolas"

firstSen    = "Tyger Tyger, burning bright\n"
secondSen   = "In the forests of the night\n"
thirdSen    = "What immortal hand or eye\n"
fourthSen   = "Could frame thy fearful symmetry?"

sentences = firstSen ++ secondSen ++
            thirdSen ++ fourthSen

-- splits on whitespace
myWords :: String -> [String]
myWords []  = []
myWords l   = x : myWords xs
    where
        x   = takeWhile (/= ' ') l
        xs  = (dropWhile (== ' ') . dropWhile (/= ' ')) l

-- splits on newline
myLines :: String -> [String]
--myLines = undefined
myLines []  = []
myLines l   = x : myLines xs
    where
        x   = takeWhile (/= '\n') l
        xs  = (dropWhile (== '\n') . dropWhile (/= '\n')) l

-- parameterizes the split character
breakOn :: String -> Char -> [String]
breakOn [] _    = []
breakOn l c     = x : breakOn xs c
    where
        x   = takeWhile (/= c) l
        xs  = (dropWhile (== c) . dropWhile (/= c)) l

shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"
    ]


main :: IO ()
main = print $
       "Are they equal? "
       ++ show (breakOn sentences '\n'
                == shouldEqual)
