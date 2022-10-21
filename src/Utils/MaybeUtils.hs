module Utils.MaybeUtils where

import Data.Foldable (foldlM)
import Prelude

-- | Takes computations returnings @Maybes@; tries each one in order.
-- The first one to return a @Just@ wins. Returns @Nothing@ if all computations
-- return @Nothing@.
firstJustsM :: (Monad m, Foldable f) => f (m (Maybe a)) -> m (Maybe a)
firstJustsM = foldlM go Nothing
  where
    go :: Monad m => Maybe a -> m (Maybe a) -> m (Maybe a)
    go Nothing action = action
    go result@(Just _) _action = return result
