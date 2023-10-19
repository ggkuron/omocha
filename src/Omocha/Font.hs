module Omocha.Font
  ( Font (..),
    loadFont,
    charToBitmap,
    metricsAscent,
    metricsDescent,
    fontBoundingBox,
    RenderedChar (..),
  )
where

import Codec.Picture
import Codec.Picture.RGBA8 (fromColorAndOpacity)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.BoundingBox
import Data.Map qualified as M
import Data.Vector.Generic qualified as VG
import Data.Vector.Storable qualified as V
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr hiding (addForeignPtrFinalizer)
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.FreeType.Internal
import Graphics.Rendering.FreeType.Internal.BBox as BB
import Graphics.Rendering.FreeType.Internal.Bitmap as B
import Graphics.Rendering.FreeType.Internal.Face as F
import Graphics.Rendering.FreeType.Internal.GlyphSlot qualified as GS
import Graphics.Rendering.FreeType.Internal.Library as L
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes as PT
import Graphics.Rendering.FreeType.Internal.Vector qualified as V
import Linear hiding (trace)
import Omocha.Bitmap
import RIO
import System.IO.Unsafe (unsafePerformIO)

data Font = Font FT_Face (Double, Double) (Box V2 Double) (IORef (M.Map (Double, Char) RenderedChar))

-- | Get the font's metrics.
metricsAscent :: Font -> Double
metricsAscent (Font _ (a, _) _ _) = a

-- | Get the font's metrics.
metricsDescent :: Font -> Double
metricsDescent (Font _ (_, d) _ _) = d

-- | Get the font's boundingbox.
fontBoundingBox :: Font -> Box V2 Double
fontBoundingBox (Font _ _ b _) = b

data RenderedChar = RenderedChar
  { bitmap :: Bitmap,
    offset :: V2 Double,
    advance :: Double,
    releaseKey :: ReleaseKey
  }

loadFont :: MonadIO m => FilePath -> m Font
loadFont path = liftIO $ alloca $ \p -> do
  runFreeType $ withCString path $ \str -> ft_New_Face freeType str 0 p
  f <- peek p
  b <- peek (bbox f)
  asc <- peek (ascender f)
  desc <- peek (descender f)
  u <- fromIntegral <$> peek (units_per_EM f)
  let box =
        (/ u)
          <$> Box
            (V2 (fromIntegral (xMin b)) (fromIntegral (yMin b)))
            (V2 (fromIntegral (xMax b)) (fromIntegral (yMax b)))
  Font f (fromIntegral asc / u, fromIntegral desc / u) box <$> newIORef M.empty

runFreeType :: IO CInt -> IO ()
runFreeType m = do
  r <- m
  unless (r == 0) $ fail $ "FreeType Error:" ++ show r

freeType :: FT_Library
{-# NOINLINE freeType #-}
freeType = unsafePerformIO $ alloca $ \p -> do
  runFreeType $ ft_Init_FreeType p
  peek p

resolutionDPI :: Int
resolutionDPI = 300

charToBitmap :: Font -> Double -> Double -> Char -> ResourceT IO RenderedChar
charToBitmap (Font face _ _ refCache) pixel adv ch = do
  let siz = pixel * 72 / fromIntegral resolutionDPI

  cache <- liftIO $ readIORef refCache
  case M.lookup (siz, ch) cache of
    Just d -> return d
    Nothing -> do
      mkChar <- liftIO $ render face siz adv ch
      key <- register $ modifyIORef refCache $ M.delete (siz, ch)
      let r = mkChar key
      liftIO $ writeIORef refCache $ M.insert (siz, ch) r cache
      return r

render :: FT_Face -> Double -> Double -> Char -> IO (ReleaseKey -> RenderedChar)
render face siz advV ch = do
  let dpi = fromIntegral resolutionDPI

  runFreeType $ ft_Set_Char_Size face 0 (floor $ siz * 64) dpi dpi
  ix <- ft_Get_Char_Index face (fromIntegral $ fromEnum ch)
  runFreeType $ ft_Load_Glyph face ix ft_LOAD_DEFAULT

  slot <- peek $ glyph face
  runFreeType $ ft_Render_Glyph slot ft_RENDER_MODE_NORMAL

  bmp <- peek $ GS.bitmap slot
  left <- fmap fromIntegral $ peek $ GS.bitmap_left slot
  top <- fmap fromIntegral $ peek $ GS.bitmap_top slot

  let h = fromIntegral $ B.rows bmp
      w = fromIntegral $ B.width bmp

  fptr <- newForeignPtr_ $ castPtr $ buffer bmp
  adv <- peek $ GS.advance slot
  b <-
    liftBitmapIO $
      fromColorAndOpacity (PixelRGB8 255 255 255) $
        Image w h $!
          VG.new . VG.clone $
            V.unsafeFromForeignPtr0 fptr (h * w)

  return $
    RenderedChar
      b
      (V2 left (advV - top))
      (fromIntegral (V.x adv) / 64)
