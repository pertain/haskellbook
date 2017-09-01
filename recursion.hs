-- Implement integral division using recursive subtraction

{-
 -- Naive and incomplete
 -- (doesn't handle denominator <= 0)
divRec :: Integral a => a -> a -> a
divRec numer denom = go numer denom 0
    where
        go n d q
            | n < d     = q
            | otherwise = go (n - d) d (q + 1)
-}

-- Wraps output in Maybe to deal with denom = 0
-- (need to add support for denom < 0)
divRec :: Integral a => a -> a -> Maybe a
divRec numer denom = go numer denom (Just 0)
    where
        go n d (Just q)
            | d == 0    = Nothing
            | n < d     = Just q
            | otherwise = go (n - d) d (Just (q + 1))

mc91 :: Integral a => a -> a
mc91 n
    | n > 100   = n - 10
    | otherwise = mc91 (mc91 (n + 11))
