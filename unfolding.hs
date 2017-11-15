-- Unfolds  -- End of chapter exercises (ch 12)

-- 1) Write the function myIterate using direct recursion.
--    The output should match that of 'iterate'.
myIterate :: (a -> a) -> a -> [a]
myIterate f a = go f [] a
    where
        go :: (a -> a) -> [a] -> a -> [a]
        go f' xs x = x : go f' (f' x : xs) (f' x)
