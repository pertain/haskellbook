-- Small library for Either -- End of chapter exercises (ch 12)

-- 1) Extract all Left values from a list of Either
--    Try to eventually arrive at a solution that uses foldr.
lefts' :: [Either a b] -> [a]
lefts' = foldr getLeft []
    where
        getLeft (Left a) acc = a : acc
        getLeft _ acc        = acc

-- 2) Same as the last one. Use foldr eventually.
rights' :: [Either a b] -> [b]
rights' = foldr getRight []
    where
        getRight (Right b) acc = b : acc
        getRight _ acc         = acc

-- 3)
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' e = (lefts' e, rights' e)

-- 4)
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just (f b)
eitherMaybe' _ (Left _)  = Nothing

-- 5) This is a general catamorphism for Either values
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)  = f a
either' _ g (Right b) = g b

-- 6) Same as before, but use the either' function you just wrote
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left _)    = Nothing
eitherMaybe'' f e@(Right b) = Just (either' (\a -> f b) f e)
