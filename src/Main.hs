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

module Main where   

import Omocha.Bitmap
import Omocha.Font
import Omocha.Collada
import Omocha.Types

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
import Control.Lens
import Data.Int(Int32)
import Data.Maybe(fromMaybe, fromJust, isJust)
import Data.Function(on)
import qualified FRP.Elerea.Param as E

loadBitmapsWith [|getDataFileName|] "../static/images"

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


windowSize  (a, _, _, _, _, _) = a
modelNorm   (_, a, _, _, _, _) = a
viewCamera  (_, _, a, _, _, _) = a 
viewTarget  (_, _, _, a, _, _) = a
viewUp      (_, _, _, _, a, _) = a


data Buffers os = Buffers {
                              normals  :: Buffer os (B3 Float),
                              board :: Buffer os (B3 Float, B2 Float),
                              boardNormals  :: Buffer os (B3 Float),
                              position :: Buffer os (B3 Float, B2 Float)
                          }

newPrimitiveArray :: Buffer os (t2, t1)
       -> Buffer os t -> Render os (PrimitiveArray Triangles (t2, t, t1))
newPrimitiveArray p n = do   
  pArr <- newVertexArray p  
  nArr <- newVertexArray n  
  return $ 
    toPrimitiveArrayInstanced 
    TriangleStrip 
    (\(p, u) n -> (p, n, u))
    pArr nArr 

 
main = runContextT (GLFW.defaultHandleConfig { GLFW.configEventPolicy = Nothing }) $ do
    win <- newWindow (WindowFormatColorDepth RGBA8 Depth16) (GLFW.defaultWindowConfig "omocha")
    uniform :: Buffer os (Uniform UniInput) <- newBuffer 1  

    position :: Buffer os (B3 Float, B2 Float) <- newBuffer 4  
    normals :: Buffer os (B3 Float) <- newBuffer 1  
    board :: Buffer os (B3 Float, B2 Float) <- newBuffer 4
    boardNormals :: Buffer os (B3 Float) <- newBuffer 1  
    tip :: Buffer os (B2 Int32) <- newBuffer 32

    writeBuffer (board) 0 [
                              (V3 1 0 1, V2 1 1),
                              (V3 1 0 (-1), V2 1 0),
                              (V3 (-1) 0 1, V2 0 1),
                              (V3 (-1) 0 (-1), V2 0 0)
                          ] 
    writeBuffer (normals) 0 [V3 0 1 0]
    writeBuffer (boardNormals) 0 [V3 0 1 0]
    let buffers = Buffers {
        normals,
        board,
        boardNormals,
        position
    }
    font <- loadFont "VL-PGothic-Regular.ttf"
    scene <- liftIO $ readColladaFile "house.dae"

    tex <- loadImage _front0_png
    btex <- loadImage _maptips_grass_png
   
    shader <- compileShader $ boardShader makeSide win uniform
    shader' <- compileShader $ boardShader makeBoard win uniform

    cscene <- renderScene scene 

    let renderings = \vpSize -> [ do   
                                    clearWindowColor win (V4 0 0.25 1 0)
                                    clearWindowDepth win 1
                                    prims <- newPrimitiveArray position normals
                                    prims' <- newPrimitiveArray board boardNormals
                                    shader' $ RenderInput vpSize prims' btex
                                    shader $ RenderInput vpSize prims tex
                                    shader $ cscene vpSize
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
        return $ renderFrame win uniform buffers renderings <$> camera' <*> target <*> fps

    liftIO $ GLFW.setTime 0
    fix $ \loop -> do
        GLFW.mainstep win GLFW.Wait
        dt <- readInput win keyInputSink
        case dt of 
            Just dt -> do
                join $ liftIO $ network dt
                loop
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
               -> Buffers os 
               -> (V2 Int -> [Render os ()])
               -> V3 Float
               -> V3 Float
               -> Double
               -> ContextT GLFW.Handle os IO ()
renderFrame win uniform buffers renderings camera target  fps = do
    size <- getFrameBufferSize win
    let viewUp = V3 0 1 0 
        normMat = identity
        uni = (fromIntegral <$> size, normMat, camera, target, viewUp, 0.0) 

    writeBuffer uniform 0 [uni]
    writeBuffer (position buffers) 0 [
                                         (target&_x+~1&_y*~(-1)&_y+~1, V2 1 1),
                                         (target&_x+~1&_y*~(-1)&_y-~1, V2 1 0),
                                         (target&_x-~1&_y*~(-1)&_y+~1, V2 0 1),
                                         (target&_x-~1&_y*~(-1)&_y-~1, V2 0 0)
                                     ] 
    mapM_ render (renderings size)
    swapWindowBuffers win

keyIsPressed win k = maybe False (== GLFW.KeyState'Pressed) <$> GLFW.getKey win k
        

makeSide :: Fractional a => (V3 a, V3 a, V2 a) -> (V3 a, V3 a, V2 a)
makeSide (p, normal, uv) = (p&_y+~1, normal, uv)

makeBoard :: Fractional a => (V3 a, V3 a, V2 a) -> (V3 a, V3 a, V2 a)
makeBoard (p, normal, uv) = (p, normal, uv)


