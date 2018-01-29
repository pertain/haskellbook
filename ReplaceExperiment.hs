module ReplaceExperiment where

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

-- More concrete
replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

-- What happens if we lift it?
liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

-- More concrete
liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

-- What if we lift twice?
twiceLifted :: (Functor f1, Functor f) =>
               f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

-- More concrete
twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

-- Thrice lifted?
thriceLifted :: (Functor f2, Functor f1, Functor f) =>
                f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

-- More concrete
thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

-- Now print the results
main :: IO ()
main = do
    putStr "replaceWithP' lms:  "
    print $ replaceWithP' lms

    putStr "liftedReplace lms:  "
    print $ liftedReplace lms

    putStr "liftedReplace' lms: "
    print $ liftedReplace' lms

    putStr "twiceLifted lms:    "
    print $ twiceLifted lms

    putStr "twiceLifted' lms:   "
    print $ twiceLifted' lms

    putStr "thriceLifted lms:   "
    print $ thriceLifted lms

    putStr "thriceLifted' lms   "
    print $ thriceLifted' lms
