-- midCh11.hs
--
-- In-Chapter exercises (ch 11)

{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- pragma needed for deriving custom typeclasses
{-# LANGUAGE FlexibleInstances #-}


-- Exercises: Vehicles
--
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


-- 1)
-- What is the type of myCar?
-- answer: Vehicle

-- 2)
-- Given the following, define the functions:
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

-- 3)
-- Now we're going to write a function to tell us
-- the manufacturer of a piece of data:
--getManu :: Vehicle -> Manufacturer
--getManu = undefined

-- Modified to handle Vehicles without Manufacturer data
getManu :: Vehicle -> Maybe Manufacturer
getManu (Car m _) = Just m
getManu _         = Nothing


-- 5)
-- Plane has been modified to include Size.
-- This function verifies the change.
getSize :: Vehicle -> Maybe Size
getSize (Plane _ s) = Just s
getSize _           = Nothing


-- Exercises: Logic Goats
--
-- PART 1
-- functions using newtype will not work with the underlying type
-- (i.e. tooManyGoats takes Goats Int, but not Int)
newtype Goats = Goats Int
    deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a
    where
        tooMany :: a -> Bool

instance TooMany Int
    where
        tooMany n = n > 12

instance TooMany Double
    where
        tooMany n = n > 13.0

instance TooMany Goats
    where
        tooMany (Goats n) = n > 15


-- PART 2
-- TooMany is derived without the need for an explicit instance
-- (requires the GeneralizedNewtypeDeriving pragma)
newtype DerivedGoats = DerivedGoats Int
    deriving (Eq, Show, TooMany)


-- 1)
--
-- This overloads class TooMany a (requires the FlexibleInstances pragma)
instance TooMany (Int, String)
    where
        tooMany (n, _) = n > 35

-- This approach sidesteps the need for the FlexibleInstances pragma
newtype GoatsByName = GoatsByName (Int, String)
    deriving (Eq, Show)

instance TooMany GoatsByName
    where
        tooMany (GoatsByName (n, _)) = n > 35


-- 2)
--
-- This overloads class TooMany a (requires the FlexibleInstances pragma)
-- Commented out because it overlaps with instances in Logic Goats (3)
--instance TooMany (Int, Int)
    --where
        --tooMany (n, m) = (n + m) > 60

-- This approach sidesteps the need for the FlexibleInstances pragma
newtype GoatsByLocation = GoatsByLocation (Int, Int)
    deriving (Eq, Show)

instance TooMany GoatsByLocation
    where
        tooMany (GoatsByLocation (n, m)) = (n + m) > 90


-- 3)
--
-- This overloads class TooMany a (requires the FlexibleInstances pragma)
-- Works with any tuple (a, a) of types (Ord a, Num a)
-- but requires explicit type declarations.
-- This is more generic than the next instance
--instance (Ord a, Num a) => TooMany (a, a)
    --where
        --tooMany (n, m) = (n + m) > 48

-- This overloads class TooMany a (requires the FlexibleInstances pragma)
-- Works with any tuple (a, a) of types (Ord a, Num a, TooMany a).
-- The thing to note here is that it only works for defined instances
-- of TooMany a (i.e. TooMany Int, or TooMany Double)
-- This is less generic than the previous instance
instance (Ord a, Num a, TooMany a) => TooMany (a, a)
    where
        tooMany (n, m) = (n + m) > 49

-- This approach sidesteps the need for the FlexibleInstances pragma
newtype WayTooMany a = WayTooMany a
    deriving (Eq, Show)

instance (Ord a, Num a) => TooMany (WayTooMany (a, a))
    where
        tooMany (WayTooMany (n, m)) = (n + m) > 49


-- Exercise: Programmers
--
data OperatingSystem = Linux | OpenBds | Mac | Windows
    deriving (Eq, Show)

data ProgLang = Haskell | Agda | Idris | PureScript
    deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem , lang :: ProgLang }
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [Linux, OpenBds, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers =
    [ Programmer { os = x, lang = y}
    | x <- allOperatingSystems
    , y <- allLanguages
    ]


-- Binary Tree
--
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node l a r)
    | b == a = Node l a r
    | b < a  = Node (insert' b l) a r
    | b > a  = Node l a (insert' b r)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node l a r) = Node (mapTree f l) (f a) (mapTree f r)

-- preorder (root left right)
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node l a r) = a : (preorder l ++ preorder r)

-- inorder (left root right)
inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node l a r) = inorder l ++ (a : inorder r)

-- postorder (left right root)
postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node l a r) = postorder l ++ postorder r ++ [a]

-- preorder implementation of foldr for binary tree
foldrTreePre :: (a -> b -> b) -> b -> BinaryTree a -> b
foldrTreePre _ z Leaf = z
foldrTreePre f z (Node l a r) = foldrTreePre f (foldrTreePre f (f a z) l) r

-- inorder implementation of foldr for binary tree
foldrTreeIn :: (a -> b -> b) -> b -> BinaryTree a -> b
foldrTreeIn _ z Leaf = z
foldrTreeIn f z (Node l a r) = foldrTreeIn f (f a (foldrTreeIn f z l)) r

-- postorder implementation of foldr for binary tree
foldrTreePost :: (a -> b -> b) -> b -> BinaryTree a -> b
foldrTreePost _ z Leaf = z
foldrTreePost f z (Node l a r) = f a (foldrTreePost f (foldrTreePost f z l) r)

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testTree1 :: BinaryTree Integer
testTree1 = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

testTree2 :: BinaryTree Integer
testTree2 = Node (Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)) 1 (Node Leaf 3 Leaf)

mapExpected1 :: BinaryTree Integer
mapExpected1 = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapExpected2 :: BinaryTree Integer
mapExpected2 = Node (Node (Node Leaf 5 Leaf) 3 (Node Leaf 6 Leaf)) 2 (Node Leaf 4 Leaf)

testMap :: IO ()
testMap =
    if mapTree (+1) testTree1 == mapExpected1
    then putStrLn "mapTree passed"
    else putStrLn "mapTree failed"

testPreorder :: IO ()
testPreorder 
    | prePassed = putStrLn "Preorder passed"
    | otherwise = putStrLn "Preorder failed"
    where
        prePassed = preorder testTree == [2, 1, 3]

testInorder :: IO ()
testInorder
    | inPassed  = putStrLn "Inorder passed"
    | otherwise = putStrLn "Inorder failed"
    where
        inPassed = inorder testTree == [1, 2, 3]

testPostorder :: IO ()
testPostorder
    | postPassed = putStrLn "Postorder passed"
    | otherwise  = putStrLn "Postorder failed"
    where
        postPassed = postorder testTree == [1, 3, 2]

testPreorderFoldr :: IO ()
testPreorderFoldr
    | prePassed = putStrLn "FoldrPre passed"
    | otherwise = putStrLn "FoldrPre failed"
    where
        prePassed = foldrTreePre (:) [] testTree == [3, 1, 2]

testInorderFoldr :: IO ()
testInorderFoldr
    | inPassed  = putStrLn "FoldrIn passed"
    | otherwise = putStrLn "FoldrIn failed"
    where
        inPassed = foldrTreeIn (:) [] testTree == [3, 2, 1]

testPostorderFoldr :: IO ()
testPostorderFoldr
    | postPassed = putStrLn "FoldrPost passed"
    | otherwise  = putStrLn "FoldrPost failed"
    where
        postPassed = foldrTreePost (:) [] testTree == [2, 3, 1]


main :: IO ()
main = do
    testMap
    testPreorder
    testInorder
    testPostorder
    testPreorderFoldr
    testInorderFoldr
    testPostorderFoldr
