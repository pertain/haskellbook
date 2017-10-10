--module BinaryTree where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node l a r)
    | b == a = Node l a r
    | b < a  = Node (insert' b l) a r
    | b > a  = Node l a (insert' b r)
