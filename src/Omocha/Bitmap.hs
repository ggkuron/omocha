{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Omocha.Bitmap
  ( Bitmap (..),
    liftBitmapIO,
    loadBitmapsWith,
    getPixels,
    getImage,
  )
where

import Codec.Picture qualified as C
import Codec.Picture.RGBA8 qualified as C
import Codec.Picture.Types qualified as C
import Control.Monad
import Control.Monad.IO.Class
import Data.Char (isAlphaNum)
import Language.Haskell.TH
import Linear
import RIO
import System.Directory
import System.FilePath
import System.IO.Unsafe
import System.Random (randomIO)
import Prelude (putStrLn)

data Bitmap = Bitmap {bitmapImage :: C.Image C.PixelRGBA8, bitmapHash :: Int}

liftBitmapIO :: MonadIO m => C.Image C.PixelRGBA8 -> m Bitmap
liftBitmapIO b = liftIO $ Bitmap b <$> randomIO

-- | Load an image file.
readBitmap :: MonadIO m => FilePath -> m Bitmap
readBitmap path = liftIO $ Bitmap <$> C.readImageRGBA8 path <*> randomIO

-- | The type of the given 'ExpQ' must be @FilePath -> IO FilePath@
-- FIXME: This may cause name duplication if there are multiple non-alphanumeric file names.
loadBitmapsWith :: ExpQ -> FilePath -> Q [Dec]
loadBitmapsWith getFullPath path = do
  loc <- ((</> path) . takeDirectory) . loc_filename <$> location
  paths <- runIO $ getFileList loc

  sequence $ do
    p <- paths
    let name = pathToName p
    [ return $ SigD (mkName name) (ConT ''Bitmap),
      funD (mkName name) [clause [] (normalB $ load name $ loc </> p) []]
      ]
  where
    load name fp = do
      runIO $ putStrLn $ "Defined: " ++ fp ++ " as `" ++ name ++ "'"

      appE (varE 'unsafePerformIO) $
        uInfixE (appE getFullPath $ litE $ StringL fp) (varE '(>>=)) (varE 'readBitmap)

getFileList :: FilePath -> IO [FilePath]
getFileList path = do
  allContents <- filter notHidden `fmap` getDirectoryContents path

  files <- filterM (doesFileExist . (path </>)) allContents
  dirs <- filterM (doesDirectoryExist . (path </>)) allContents
  fmap ((files ++) . concat) $ forM dirs $ \i -> map (i </>) `fmap` getFileList (path </> i)
  where
    notHidden ('.' : _) = False
    notHidden _ = True

pathToName :: FilePath -> String
pathToName = ('_' :) . map p
  where
    p c
      | isAlphaNum c = c
      | otherwise = '_'

getPixels :: C.Image C.PixelRGBA8 -> [V4 C.Pixel8]
getPixels = C.pixelFold getJuicyPixel []
  where
    getJuicyPixel xs _ _ pix = let C.PixelRGBA8 r g b a = C.convertPixel pix in V4 r g b a : xs

getImage :: Bitmap -> (V2 Int, C.Image C.PixelRGBA8)
getImage bmp =
  let img = bmp.bitmapImage
      siz = V2 (C.imageWidth img) (C.imageHeight img)
   in (siz, img)
