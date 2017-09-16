-- Database Processing exercise (ch 10)

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime (fromGregorian 1911 5 1)
                      (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Holas Nolas"
    , DbDate (UTCTime (fromGregorian 1921 5 1)
                      (secondsToDiffTime 34123))
    ]

-- 1) Write a function that filters for DbDate values
--    and returns a list of UTCTime values inside them.
filterDbDate :: [DatabaseItem] -> [UTCTime]
--filterDbDate = undefined
filterDbDate []                 = []
filterDbDate ((DbDate (x)):xs)  = x : filterDbDate xs
filterDbDate (_:xs)             = filterDbDate xs

-- 2) Write a function that filters for DbNumber values
--    and returns a list of the Integer values inside them.
filterDbNumber :: [DatabaseItem] -> [Integer]
--filterDbNumber = undefined
filterDbNumber []                   = []
filterDbNumber ((DbNumber (x)):xs)  = x : filterDbNumber xs
filterDbNumber (_:xs)               = filterDbNumber xs

-- 3) Write a function that gets the most recent date
mostRecent :: [DatabaseItem] -> UTCTime
--mostRecent = undefined
mostRecent = maximum . filterDbDate

-- 4) Write a function that sums all of the DbNumber values
sumDb :: [DatabaseItem] -> Integer
--sumDb = undefined
sumDb = sum . filterDbNumber

-- 5) Write a function that gets the average of the DbNumber values
avgDb :: [DatabaseItem] -> Double
--avgDb = undefined
avgDb xs = total / count
    where
        count = (fromIntegral . length) (filterDbNumber xs)
        total = fromIntegral (sumDb xs)
