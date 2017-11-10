-- Small Library for Maybe -- End of chapter exercises (ch 12)

-- 1) Simple boolean checks for Maybe values
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just _) = False

-- 2) The following is a Maybe catamorphism. You can turn
--    a Maybe value into anything else with this.
mayybe :: b -> (a -> b) -> Maybe a -> b
mayybe b _ Nothing  = b
mayybe _ f (Just a) = f a

-- 3)
fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  = a
fromMaybe _ (Just a) = a

-- 4) Converting between List and Maybe
listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

-- 5) Drop Nothings from list
catMaybes :: [Maybe a] -> [a]
catMaybes []            = []
catMaybes (Nothing:xs)  = catMaybes xs
catMaybes ((Just x):xs) = x : catMaybes xs

-- 6) You'll see this called "sequence" later.
flipMaybe :: Eq a => [Maybe a] -> Maybe [a]
flipMaybe ms = case elem Nothing ms of
    True -> Nothing
    _    -> Just (flipMaybe' ms)
    where
        flipMaybe' [] = []
        flipMaybe' ((Just x):xs) = x : flipMaybe' xs
