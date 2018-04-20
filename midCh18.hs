-- midCh18.hs
--
-- In-chapter exercises (ch 18)

import Control.Monad (join)

-- The answer is the exercise
--
-- Write bind in terms of fmap and join
--
bind :: Monad m => (a -> m b) -> m a -> m b
--bind = undefined
bind f ma = join (f <$> ma)
