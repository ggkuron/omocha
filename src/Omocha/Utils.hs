{-# LANGUAGE TemplateHaskell, QuasiQuotes, CPP #-}

module Omocha.Utils (
  loadBitmapsWith
, Bitmap(..)
) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Language.Haskell.TH
import Control.Monad
import Control.Monad.IO.Class

import qualified Codec.Picture as C
import qualified Codec.Picture.RGBA8 as C

-- import Linear
import System.Random
import System.Directory
import System.FilePath
import System.IO.Unsafe
-- import System.Random
import System.Environment
-- import FreeGame.UI
import Data.Hashable
import qualified Data.IntMap as IM
import Data.Char

newtype Bitmap = Bitmap { bitmapImage :: C.Image C.PixelRGBA8}

-- | Load an image file.
readBitmap :: MonadIO m => FilePath -> m Bitmap
readBitmap path = liftIO $ do
    putStrLn path
    Bitmap <$> C.readImageRGBA8 path 


-- | The type of the given 'ExpQ' must be @FilePath -> IO FilePath@
-- FIXME: This may cause name duplication if there are multiple non-alphanumeric file names.
loadBitmapsWith :: ExpQ -> FilePath -> Q [Dec]
loadBitmapsWith getFullPath path = do
    loc <- (</>path) <$> takeDirectory <$> loc_filename <$> location
    paths <- runIO $ getFileList loc

    sequence $ do
        p <- paths
        let name = pathToName p
        [ return $ SigD (mkName name) (ConT ''Bitmap)
            , funD (mkName name) [clause [] (normalB $ load name $ loc </> p) []]
            ]
    where
        load name fp = do
            runIO $ putStrLn $ "Defined: " ++ fp ++ " as `" ++ name ++ "'"
            
            appE (varE 'unsafePerformIO) $
                    uInfixE (appE getFullPath $ litE $ StringL fp) (varE '(>>=)) (varE 'readBitmap)

getFileList :: FilePath -> IO [FilePath]
getFileList path = do
    allContents <- filter notHidden `fmap` getDirectoryContents path

    files <- filterM (doesFileExist . (path</>)) allContents
    dirs <- filterM (doesDirectoryExist . (path</>)) allContents
    fmap ((files++).concat) $ forM dirs $ \i -> map (i</>) `fmap` getFileList (path</>i)
    where
        notHidden ('.':_) = False
        notHidden _ = True

pathToName :: FilePath -> String
pathToName = ('_':) . map p where
    p c | isAlphaNum c = c
        | otherwise = '_'


