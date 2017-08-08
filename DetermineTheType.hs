{-# LANGUAGE NoMonomorphismRestriction #-}

-- Each function below omits type declarations
-- and force the compiler to infer.
-- The NoMonomorphism pragma forces each type to
-- be as polymorphic as possible.

module DetermineTheType where

-- simple example
-- Without the NoMonomorphismRestriction pragma
-- GHC would infer the type 'Integer'.
-- However, the most polymorphic type that this
-- function can have is 'Num t'.
example = 1


-- 1)   All function applications return a value.
--      Determine the value returned by these
--      function applications and the type of
--      that value.
f1 = (*9) 6

f2 = head [(0,"doge"),(1,"kitteh")]

f3 = head [(0 :: Integer,"doge"),(1,"kitteh")]

f4 = if False then True else False

f5 = length [1,2,3,4,5]

f6 = (length [1,2,3,4]) > (length "TACOCAT")


----------------------------------
-- Given:
x = 5
y = x + 5

-- 2)   What is the type of w?
w = y * 10

-- 3)   What is the type of z?
z y = y * 10

-- 4)   What is the type of f1?
ff = 4 / y
----------------------------------


-- 5)   What is the type of f2?
x2  = "Julie"
y2  = " <3 "
z2  = "Haskell"
fff = x2 ++ y2 ++ z2
