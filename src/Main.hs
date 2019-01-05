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

type UniInput = (B2 Int32, V3 (B3 Float), B3 Float, B3 Float, B3 Float, B Float)

windowSize  (a, _, _, _, _, _) = a
modelNorm   (_, a, _, _, _, _) = a
viewCamera  (_, _, a, _, _, _) = a 
viewTarget  (_, _, _, a, _, _) = a
viewUp      (_, _, _, _, a, _) = a


data RenderInput os = RenderInput {
                        riScreenSize :: V2 Int, 
                        riStream :: PrimitiveArray Triangles (B3 Float, B3 Float, B2 Float),
                        tex :: Texture2D os (Format RGBAFloat)
                      }
                    | BoardInput {
                        riScreenSize :: V2 Int, 
                        riStream :: PrimitiveArray Triangles (B3 Float, B3 Float, B2 Float),
                        tex :: Texture2D os (Format RGBAFloat)
                      }

data Buffers os = Buffers {
                              normals  :: Buffer os (B3 Float),
                              board :: Buffer os (B3 Float, B2 Float),
                              boardNormals  :: Buffer os (B3 Float),
                              position :: Buffer os (B3 Float, B2 Float)
                          }

 
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
    let makePrimitives p n = do   
          pArr <- newVertexArray p  
          nArr <- newVertexArray n  
          return $ 
            toPrimitiveArrayInstanced 
            TriangleStrip 
            (\(p, u) n -> (p, n, u))
            pArr nArr 

    font <- loadFont "VL-PGothic-Regular.ttf"
    scene <- liftIO $ readColladaFile "house.dae"

    tex <- loadImage _front0_png
    btex <- loadImage _maptips_grass_png
   
    shader <- compileShader $ boardShader makeSide win uniform
    shader' <- compileShader $ boardShader makeBoard win uniform

    let renderings = \size -> [ do   
                                  clearWindowColor win (V4 0 0.25 1 0)
                                  clearWindowDepth win 1
                                  prims <- makePrimitives position normals
                                  prims' <- makePrimitives board boardNormals
                                  shader' $ RenderInput size prims' btex
                                  shader $ RenderInput size prims tex
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
        
boardShader :: ((V3 VFloat, V3 VFloat, V2 VFloat) -> (V3 (S V Float), V3 VFloat, V2 (S V Float))) 
               -> Window os RGBAFloat Depth 
               -> Buffer os (Uniform UniInput) 
               -> Shader os (RenderInput os) ()
boardShader pick win uniform = do
    uni <- getUniform (const (uniform, 0))
    boards <- fmap pick <$> toPrimitiveStream riStream
    let projectedSides = proj uni <$> boards
        filterMode = SamplerFilter Linear Linear Linear (Just 16)
        edge = (pure ClampToEdge, 1.0)
    samp <- newSampler2D $ \ri -> (tex ri, filterMode, edge)

    uv <- rasterize (\ri -> (Front, ViewPort (V2 0 0) (riScreenSize ri), DepthRange 0 1) ) projectedSides
    let litFrags = light samp <$> uv
        litFragsWithDepth = withRasterizedInfo
                               (\p x -> (p, (rasterizedFragCoord x)^._z)) litFrags
        colorOption = ContextColorOption (BlendRgbAlpha (FuncAdd, FuncAdd) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors Zero One) (V4 0 0 0 0)) (pure True)
        depthOption = DepthOption Lequal True
    
    drawWindowColorDepth (const (win, colorOption, depthOption)) litFragsWithDepth


light :: ColorSampleable c => Sampler2D (Format c) -> (t, V2 (S F Float)) -> ColorSample F c
light samp (normal, uv) = sample2D samp SampleAuto (Just 1) Nothing uv 

makeSide :: Fractional a => (V3 a, V3 a, V2 a) -> (V3 a, V3 a, V2 a)
makeSide (p, normal, uv) = (p&_y+~1, normal, uv)

makeBoard :: Fractional a => (V3 a, V3 a, V2 a) -> (V3 a, V3 a, V2 a)
makeBoard (p, normal, uv) = (p, normal, uv)

proj uni (V3 px py pz, normal, uv) =   
    let modelViewProj = perspective (pi/3) (let V2 w h = windowSize uni in (toFloat w) / (toFloat h)) 1 (-1)
        normMat = modelNorm uni
        viewProj = lookAt' (viewCamera uni) (viewTarget uni) (viewUp uni)
        in (modelViewProj !*! viewProj !* V4 px py pz 1, (fmap Flat $ normMat !* normal, uv))   

lookAt' eye center up =
    V4 (V4 (xa^._x)  (xa^._y)  (xa^._z)  xd)
       (V4 (ya^._x)  (ya^._y)  (ya^._z)  yd)
       (V4 (-za^._x) (-za^._y) (-za^._z) zd)
       (V4 0         0         0          1)
    where za = signorm $ center - eye
          xa = signorm $ cross za up
          ya = cross xa za
          xd = -dot xa eye
          yd = -dot ya eye
          zd = dot za eye

