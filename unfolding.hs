-- Unfolds  -- End of chapter exercises (ch 12)

-- 1) Write the function myIterate using direct recursion.
--    The output should match that of 'iterate'.
myIterate :: (a -> a) -> a -> [a]
myIterate f x = go f [] x
    where
        go :: (a -> a) -> [a] -> a -> [a]
        go f' xs x' = x' : go f' (f' x' : xs) (f' x')

-- 2) Write the function myUnfoldr using direct recursion.
--    The output should match that of 'unfoldr'.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = go f [] x
    where
        go :: (b -> Maybe (a, b)) -> [a] -> b -> [a]
        go f' ys x' = case f' x' of
            Nothing       -> []
            Just (y, x'') -> y : go f' (y : ys) x''

-- 3) Rewrite myIterate into betterIterate using myUnfoldr.
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x' -> Just (x', f x')) x


-- Unfold for Binary Tree

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Show)

-- 1) Write unfold for BinaryTree
unfoldBT :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfoldBT f x = go f Leaf x
    where
        go :: (a -> Maybe (a, b, a)) -> BinaryTree b -> a -> BinaryTree b
        go f' bt x' = case f' x' of
            Nothing          -> Leaf
            Just (xl, y, xr) -> Node (go f' bt xl) y (go f' bt xr)

-- 2) Write treeBuild using unfoldBT
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfoldBT go 0
    where
        go :: Integer -> Maybe (Integer, Integer, Integer)
        go i = case i < n of
            True  -> Just (i + 1, i, i + 1)
            False -> Nothing
