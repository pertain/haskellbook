-- endCh20.hs
--
-- End of chapter exercises (ch 20)


-- Write Foldable instances

-- 1)
data Constant a b = Constant b
    deriving (Eq, Show)

instance Foldable (Constant a) where
    foldr f z (Constant a) = f a z
    foldMap f (Constant a) = f a


-- 2)
data Two a b = Two a b
    deriving (Eq, Show)

instance Foldable (Two a) where
    foldr f z (Two _ b) = f b z
    foldMap f (Two _ b) = f b


-- 3)
data Three a b c = Three a b c
    deriving (Eq, Show)

instance Foldable (Three a b) where
    foldr f z (Three _ _ c) = f c z
    foldMap f (Three _ _ c) = f c


-- 4)
data Three' a b = Three' a b b
    deriving (Eq, Show)

instance Foldable (Three' a) where
    foldr f z (Three' _ _ b) = f b z
    foldMap f (Three' _ _ b) = f b


-- 5)
data Four' a b = Four' a b b b
    deriving (Eq, Show)

instance Foldable (Four' a) where
    foldr f z (Four' _ _ _ b) = f b z
    foldMap f (Four' _ _ _ b) = f b


-- Write a filter function for Foldable
-- types using foldMap
filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF = undefined
