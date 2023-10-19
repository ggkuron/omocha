{-# LANGUAGE DeriveFunctor #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Omocha.Text (TextF (..), TextT, runTextT, text) where

import Codec.Picture qualified as P
import Codec.Picture.Types qualified as P
import Control.Lens ((+=), (.=))
import Control.Monad.State
import Control.Monad.Trans.Free
import Control.Monad.Trans.Resource
import Data.BoundingBox
import Data.String
import Linear hiding (trace)
import Omocha.Bitmap (Bitmap (bitmapImage))
import Omocha.Font
import RIO

data TextF a = TypeChar Char a deriving (Functor)

type TextT = FreeT TextF

instance Monad m => IsString (TextT m ()) where
  fromString = mapM_ $ \c -> liftF (TypeChar c ())

type Renderer m a = (Char -> Box V2 Double -> Bitmap -> m a)

-- | Render a 'TextT'.
runTextT :: (MonadResource m) => Maybe (Box V2 Double) -> Font -> Double -> Renderer m a -> TextT m () -> m [a]
runTextT bbox font siz render = flip evalStateT (V2 x0 y0) . go []
  where
    go a m =
      lift (runFreeT m) >>= \r -> case r of
        Pure p -> return a
        Free (TypeChar '\n' cont) -> do
          _x .= x0
          _y += advV
          go a cont
        Free (TypeChar ch cont) -> do
          RenderedChar bmp offset adv _ <- liftResourceT $ charToBitmap font siz advV ch
          pen <- get
          let pen' = over _x (+ adv) pen
              pen'' =
                if cond pen'
                  then pen'
                  else V2 x0 (view _y pen + advV)
          let p = pen + offset
          bb <- lift $ render ch (Box p (p + fmap fromIntegral (V2 (P.imageWidth . (.bitmapImage) $ bmp) (P.imageHeight bmp.bitmapImage)))) bmp
          put pen''
          go (a ++ [bb]) cont
    advV = siz * (metricsAscent font - metricsDescent font) * 1.1
    (V2 x0 y0, cond) = maybe (zero, const True) (\b -> (b ^. position zero, flip isInside b)) bbox

-- | Render a 'String'.
text :: (MonadResource m, Monad m) => Font -> Double -> Renderer m a -> Maybe (Box V2 Double) -> String -> m [a]
text font siz renderer bbox str = runTextT bbox font siz renderer (fromString str)