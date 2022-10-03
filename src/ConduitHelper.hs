-- |
module ConduitHelper where

import ClassyPrelude.Conduit

idC :: MonadIO m => ConduitT b b m ()
idC = takeWhileC (const True)
