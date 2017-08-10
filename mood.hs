data Mood = Blah | Woot
    --deriving (Eq, Show)

instance Eq Mood
    where
        (==) Blah Blah  = True
        (==) Woot Woot  = True
        (==) _ _        = False

instance Show Mood
    where
        show Blah = "Blah"
        show Woot = "Woot"

--changeMood :: Mood -> Mood    -- These are identical
changeMood :: (->) Mood Mood    -- type declarations
changeMood m
    | m == Blah = Woot
    | otherwise = Blah
