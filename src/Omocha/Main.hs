{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Main where   

import Omocha.Bitmap
import Omocha.Font
import Omocha.Collada
import Omocha.Scene

import Paths_omocha
import Graphics.GPipe 
import qualified Graphics.GPipe.Context.GLFW as GLFW  
import qualified Graphics.GPipe.Context.GLFW.Input as Input

import qualified Codec.Picture as P
import qualified Codec.Picture.Types as P
import Control.Applicative
import Control.Monad (unless, join, when)
import Control.Monad.IO.Class (liftIO)  
import Control.Monad.Fix (fix)
import Control.Monad.Exception (MonadException)
import Control.Lens
import Data.Int(Int32)
import Data.Word (Word32,Word8)
import Data.Maybe(fromMaybe, fromJust, isJust)
import Data.Function(on)
import Data.List (nub)
import Data.Vector(Vector)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector as V 
import qualified Linear.Vector as V
import Control.Monad.IO.Class (MonadIO)

import qualified FRP.Elerea.Param as E

loadBitmapsWith [|getDataFileName|] "../../static/images"

loadImage :: (MonadIO m, ContextHandler ctx) =>
    Bitmap -> ContextT ctx os m (Texture2D os (Format RGBAFloat))
loadImage bmp = do 
       tex <- newTexture2D RGBA8 size maxBound
       writeTexture2D tex 0 0 size $ P.pixelFold getJuicyPixel [] img 
       return tex
    where (size, img) = getImage bmp

getImage :: Bitmap -> (V2 Int, P.Image P.PixelRGBA8)
getImage bmp = let img = bitmapImage bmp
                   size = V2 (P.imageWidth img) (P.imageHeight img)
                in (size, img)

getJuicyPixel xs _ _ pix = let P.PixelRGBA8 r g b a = P.convertPixel pix in V4 r g b a : xs   



scene :: Scene
scene = Scene {
          camera = V3 0 0.25 1,
          meshes = [
                        Mesh [ DrawVertex (V3   1  0   1)  (V3 0 1 0) (V2 1 1),
                               DrawVertex (V3   1  0 (-1)) (V3 0 1 0) (V2 1 0),
                               DrawVertex (V3 (-1) 0   1)  (V3 0 1 0) (V2 0 1),
                               DrawVertex (V3 (-1) 0 (-1)) (V3 0 1 0) (V2 0 0) ]
                             Nothing  
                             (V3 0 0 0)
                             Nothing
                             BoardShader
                        ,
                        Mesh [ DrawVertex (V3   1    1  0) (V3 0 1 0) (V2 1 1),
                               DrawVertex (V3   1  (-1) 0) (V3 0 1 0) (V2 1 0),
                               DrawVertex (V3 (-1)   1  0) (V3 0 1 0) (V2 0 1),
                               DrawVertex (V3 (-1) (-1) 0) (V3 0 1 0) (V2 0 0) ]
                             Nothing  
                             (V3 0 1 0)
                             Nothing
                             TargetBoard
                   ]
      }


buildRendering :: forall ctx os m tag. (ContextHandler ctx, MonadIO m, MonadException m) => 
               Window os RGBAFloat Depth
               -> Buffer os (Uniform UniInput)
               -> Scene 
               -> ContextT ctx os m (CompiledShader os (V2 Int))
buildRendering win uniform Scene{ meshes } = do
    bs <- compileShader $ boardShader (\_ -> id) win uniform
    ts <- compileShader $ boardShader (\uni -> \(p, n, uv) -> (p + viewTarget uni, n, uv)) win uniform
    ms <- compileShader $ monoShader (\_ -> id) win uniform
    rr <- mapM (renderMesh bs ts ms) meshes
    return $ \vpSize -> mapM_ (\r -> r vpSize) rr
    where 
        renderMesh :: (CompiledShader os (RenderInput os TextureInput)) 
                   -> (CompiledShader os (RenderInput os TextureInput)) 
                   -> (CompiledShader os (RenderInput os PlainInput)) 
                   -> Mesh 
                   -> ContextT ctx os m (CompiledShader os (V2 Int))
        renderMesh bs ts ms Mesh{..} = do
            vbuf :: Buffer os VBuffer <- newBuffer (length vertices)
            writeBuffer vbuf 0 [(dvPosition + offset, dvNormal, dvUv) | DrawVertex {..} <- vertices] 
            ibuf <- case indices of
                Just indices -> do
                    ibuf :: Buffer os (B Word32) <- newBuffer (length indices)
                    writeBuffer ibuf 0 $ map fromIntegral $ indices
                    return $ Just ibuf
                _ -> return Nothing

            case textureImage of
                Just tex -> do 
                    tex <- loadImage tex
                    let shader = case shaderType of
                                   BoardShader -> bs
                                   TargetBoard -> ts
                    return $ \vpSize -> do 
                                 prims <- newPrimitiveArray vbuf ibuf
                                 shader $ RenderInput vpSize prims tex
                otherwise -> do
                    let shader = ms
                    return $ \vpSize -> do 
                                 prims <- newPrimitiveArray vbuf ibuf
                                 shader $ PlainInput vpSize prims

        newPrimitiveArray :: forall b i a. (BufferFormat b, Integral i, IndexFormat b ~ i) => Buffer os a -> Maybe (Buffer os b) -> Render os (PrimitiveArray Triangles a)
        newPrimitiveArray p Nothing = do   
          pArr <- newVertexArray p  
          return $ toPrimitiveArray TriangleStrip pArr
        newPrimitiveArray p (Just i) = do   
          pArr <- newVertexArray p  
          iArr <- newIndexArray i Nothing
          return $ toPrimitiveArrayIndexed TriangleStrip iArr pArr
        

 
main = runContextT (GLFW.defaultHandleConfig { GLFW.configEventPolicy = Nothing }) $ do
    win <- newWindow (WindowFormatColorDepth RGBA8 Depth16) (GLFW.defaultWindowConfig "omocha")
    uniform :: Buffer os (Uniform UniInput) <- newBuffer 1  

    -- font <- loadFont "VL-PGothic-Regular.ttf"
    collada <- liftIO $ readColladaFile "untitled.dae"
    let scene = sceneFromCollada collada

    liftIO $ do 
        putStrLn (show $ collada)
        putStrLn (show $ scene)

    r <- buildRendering win uniform scene

    let renderings = [\vpSize -> do   
                         clearWindowColor win (V4 0 0.25 1 0)
                         clearWindowDepth win 1 
                         r vpSize 
                     ]

    (keyInput, keyInputSink) <- liftIO $ E.external (False, False, False, False)

    network <- liftIO $ E.start $ do
        frameCount <- E.stateful 0 (const (+1))
        fps <- do
          sig <- E.transfer 
                    (30, (0,0)) 
                    (\dt v (x, (v0, t)) -> let t' = dt + t
                                         in if t' > 1 then ((v-v0)/t', (v, 0)) 
                                                      else (x, (v0, t'))
                    )
                    frameCount
          return (fst <$> sig)
        moveUnit <- E.stateful 0 (\dt _ -> realToFrac $ 1.25 * dt )
        target <- E.transfer2 (V3 0 0 0)
                              (\_ (keyH, keyJ, keyK, keyL) mu t -> 
                                  t&_x+~(if 
                                          | keyL -> -mu
                                          | keyH -> mu 
                                          | otherwise ->  0)
                                   &_z+~(if 
                                          | keyJ -> -mu
                                          | keyK -> mu
                                          | otherwise ->  0)
                              ) 
                              keyInput moveUnit
        camera' <- E.transfer (V3 0 0.25 1) 
                              (\_ t camera -> 
                                  t&_y+~(5)
                                   &_z-~(7.5) 
                              ) 
                              target
        return $ renderFrame win uniform renderings <$> camera' <*> target <*> fps

    liftIO $ GLFW.setTime 0
    fix $ \loop -> do
        GLFW.mainstep win GLFW.Wait
        dt <- readInput win keyInputSink
        case dt of 
            Just dt -> (join . liftIO . network $ dt) >> loop
            Nothing -> return ()
    

readInput win keyInputSink = do
    t' <- liftIO $ do
        t <- GLFW.getTime
        GLFW.setTime 0
        return t

    closeRequested <- GLFW.windowShouldClose win 
    closeKeyPressed <- keyIsPressed win GLFW.Key'Q

    keyInput <- (,,,) <$> keyIsPressed win GLFW.Key'H
                      <*> keyIsPressed win GLFW.Key'J
                      <*> keyIsPressed win GLFW.Key'K
                      <*> keyIsPressed win GLFW.Key'L
    liftIO $ keyInputSink keyInput 
    return $ if closeRequested == Just True || closeKeyPressed then Nothing else t'


renderFrame :: Window os RGBAFloat Depth
               -> Buffer os (Uniform UniInput)
               -> [CompiledShader os (V2 Int)]
               -> V3 Float
               -> V3 Float
               -> Double
               -> ContextT GLFW.Handle os IO ()
renderFrame win uniform renderings camera target fps = do
    size <- getFrameBufferSize win
    let viewUp = V3 0 1 0 
        normMat = identity
        uni = (fromIntegral <$> size, normMat, camera, target, viewUp, 0.0) 

    writeBuffer uniform 0 [uni]

    mapM_ (\r -> render $ r size) renderings
    swapWindowBuffers win

keyIsPressed win k = maybe False (== GLFW.KeyState'Pressed) <$> GLFW.getKey win k
        

