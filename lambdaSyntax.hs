-- Practice writing anonymous lambda syntax (ch 07)
-- 
-- a) Rewrite the f function in the where clause
addOneIfOdd n = case odd n of
    True    -> f
    False   -> n
    --where f n = n + 1
    where f = n + 1
