-- Vehicles exercises (ch 11)

-- Provided datatypes
data Price = Price Integer
    deriving (Eq, Show)

-- Added for exercise 5
data Size = Small | Medium | Large | Jumbo
    deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata
    deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited
    deriving (Eq, Show)

--data Vehicle = Car Manufacturer Price | Plane Airline
    --deriving (Eq, Show)

-- Modified for exercise 5
data Vehicle = Car Manufacturer Price | Plane Airline Size
    deriving (Eq, Show)

-- Sample data
myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir Medium


-- 1) What is the type of myCar?
--      answer: Vehicle


-- 2) Given the following, define the functions:
isCar :: Vehicle -> Bool
--isCar = undefined
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
--isPlane = undefined
isPlane (Plane _ _) = True
isPlane _           = False

areCars :: [Vehicle] -> [Bool]
--areCars = undefined
areCars = map isCar


-- 3) Now we're going to write a function to tell us
--    the manufacturer of a piece of data:
--getManu :: Vehicle -> Manufacturer
--getManu = undefined

-- Modified to handle Vehicles without Manufacturer data
getManu :: Vehicle -> Maybe Manufacturer
getManu (Car m _) = Just m
getManu _         = Nothing


-- 5) Plane has been modified to include Size
--    This function verifies the change
getSize :: Vehicle -> Maybe Size
getSize (Plane _ s) = Just s
getSize _           = Nothing
