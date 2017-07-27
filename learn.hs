-- learn.hs

module Learn where

x :: Int
x = 10 * 5 + y

myResult :: Int
myResult = x * 5

y :: Int
y = 10

foo :: Int -> Int
foo x =
    let y = x * 2
        z = x ^ 2
    in 2 * y * z
