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

testTree' :: BinaryTree Integer
testTree' = Node (Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)) 1 (Node Leaf 3 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = Node (Node (Node Leaf 5 Leaf) 3 (Node Leaf 6 Leaf)) 2 (Node Leaf 4 Leaf)

mapOkay =
    if mapTree (+1) testTree' == mapExpected
    then print "Correct"
    else error "Not Correct"

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
