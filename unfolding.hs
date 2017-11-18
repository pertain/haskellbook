-- Unfolds  -- End of chapter exercises (ch 12)

-- 1) Write the function myIterate using direct recursion.
--    The output should match that of 'iterate'.
myIterate :: (a -> a) -> a -> [a]
myIterate f a = go f [] a
    where
        go :: (a -> a) -> [a] -> a -> [a]
        go f' xs x = x : go f' (f' x : xs) (f' x)

-- 2) Write the function myUnfoldr using direct recursion.
--    The output should match that of 'unfoldr'.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = go f [] b
    where
        go :: (b -> Maybe (a, b)) -> [a] -> b -> [a]
        go f' as b' = case f' b' of
            Nothing       -> []
            Just (a, b'') -> a : go f' (a : as) b''

-- 3) Rewrite myIterate into betterIterate using myUnfoldr.
betterIterate :: (a -> a) -> a -> [a]
betterIterate f a = myUnfoldr (\a -> Just (a, f a)) a
