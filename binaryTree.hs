--module BinaryTree where

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
