-- endCh17.hs
--
-- End of chapter exercises (ch 17)


-- Specialize the types
--
-- Given a type that has instances of Applicative,
-- specialize the types of the following methods:
--      pure  :: a -> ? a
--      (<*>) :: ? (a -> b) -> ? a -> ? b

-- 1) Type: []
--
listPure :: a -> [a]
listPure = pure

listApply :: [a -> b] -> [a] -> [b]
listApply = (<*>)

-- 2) Type: IO
--
ioPure :: a -> IO a
ioPure = pure

ioApply :: IO (a -> b) -> IO a -> IO b
ioApply = (<*>)

-- 3) Type: (,) a
--
pttPure :: Monoid t => b -> (t, b)
pttPure = pure

pttApply :: Monoid t => (t, a -> b) -> (t, a) -> (t, b)
pttApply = (<*>)

-- 4) Type: (->) e
--
arPure :: a -> (e -> a)
arPure = pure

arApply :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
arApply = (<*>)
