-- 8.   Write a function that tells you whether or not a given String (or
--      list) is a palindrome. Here you'll want to use a function called
--      'reverse,' a predefined function that does just what it sounds like.

isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = all (uncurry (==)) (zip l (reverse l))


-- 9.   Write a function to return the absolute value of a number using
--      if-then-else
myAbs :: Integer -> Integer
myAbs i =
    if i >= 0
    then i
    else negate i


-- 10.  Fill in the definition of the following function using fst and
--      snd:
--
--      f :: (a, b) -> (c, d) -> ((b, d), (a, c))

f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f x y = ((snd x, snd y), (fst x, fst y))

-- an alternative that uses pattern matching
tupSwap :: (a,b) -> (c,d) -> ((b,d), (a,c))
tupSwap (a,b) (x,y) = ((b,y), (a,x))
