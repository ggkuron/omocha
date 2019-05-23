{-# LANGUAGE Rank2Types, FlexibleInstances #-}

module Omocha.Class where

import           Omocha.Internal.Finalizer
import           Control.Monad.IO.Class


class FromFinalizer m where
    fromFinalizer :: FinalizerT IO a -> m a

instance FromFinalizer (FinalizerT IO) where
    fromFinalizer = id

-- | 'liftIO'@variety for 'FromFinalizer'.
embedIO :: FromFinalizer m => IO a -> m a
embedIO m = fromFinalizer (liftIO m)
{-# INLINE embedIO #-}
