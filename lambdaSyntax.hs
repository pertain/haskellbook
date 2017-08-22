{-# LANGUAGE NoMonomorphismRestriction #-}

-- 3. Practice writing anonymous lambda syntax (ch 07)
-- 
--  a) Rewrite the f function in the where clause.
addOneIfOdd n = case odd n of
    True    -> f n
    False   -> n
    --where f n = n + 1     -- original
    where f = \n -> n + 1   -- using lambda


--  b) Rewrite the following to use anonymous lambda syntax:
--addFive x y = (if x > y then y else x) + 5        -- original
addFive = (\x y -> (if x > y then y else x) + 5)    -- using lambda
--addFive = (\x y -> (min x y) + 5)                 -- alternative lambda


--  c) Rewrite the following so that it doesn't use anonymous lambda syntax
--mflip f = \x -> \y -> f y x   -- original
mflip f x y = f y x             -- w/o lambda
