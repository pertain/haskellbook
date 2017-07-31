data Mood = Blah | Woot deriving (Eq, Show)

changeMood :: Mood -> Mood
changeMood m
    | m == Blah = Woot
    | otherwise = Blah
