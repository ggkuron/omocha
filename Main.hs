{-# LANGUAGE ScopedTypeVariables
           , TypeFamilies
           , TemplateHaskell
           , QuasiQuotes
           , MultiWayIf
           , ScopedTypeVariables
           , FlexibleContexts #-}   

module Main where   

import Omocha.Utils

import Paths_omocha
import Graphics.GPipe   
import qualified Graphics.GPipe.Context.GLFW as GLFW  

import qualified Codec.Picture as P
import qualified Codec.Picture.Types as P
import Control.Monad (unless)  
import Control.Lens
import Data.Int(Int32)

loadBitmapsWith [|getDataFileName|] "static/images"

loadImage bmp = do 
       tex <- newTexture2D RGBA8 size maxBound
       writeTexture2D tex 0 0 size $ P.pixelFold getJuicyPixel [] img 
       return tex
    where (size, img) = getImage bmp

getImage :: Bitmap -> (V2 Int, P.Image P.PixelRGBA8)
getImage bmp = let img = bitmapImage bmp
                   size = V2 (P.imageWidth img) (P.imageHeight img)
                in (size, img)

getJuicyPixel xs _ _ pix =  
  let P.PixelRGBA8 r g b a = P.convertPixel pix in V4 r g b a : xs   

type UniInput = (B2 Int32, V3 (B3 Float), B3 Float, B3 Float, B3 Float)

windowSize  (a, _, _, _, _) = a
modelNorm   (_, a, _, _, _) = a
viewCamera  (_, _, a, _, _) = a 
viewTarget  (_, _, _, a, _) = a
viewUp      (_, _, _, _, a) = a

type FrameBufferColorFormat = RGBAFloat

data  RenderInput os = RenderInput 
                     {  riScreenSize :: V2 Int
                     ,  riStream :: PrimitiveArray Triangles (B3 Float, B3 Float)
                     ,  tex :: Texture2D os (Format FrameBufferColorFormat)
                     }

main = runContextT GLFW.newContext (ContextFormatColorDepth RGBA8 Depth16) $ do
    uniform :: Buffer os (Uniform UniInput) <- newBuffer 1  
   
    shader <- compileShader $ pictureShader uniform
    shader' <- compileShader $ boardShader uniform
    loop (shader, shader') uniform (V3 0 1 (-20))
    where 
        pictureShader :: Buffer os (Uniform UniInput) -> Shader os (ContextFormat FrameBufferColorFormat Depth) (RenderInput os) ()
        pictureShader uniform = do
            uni <- getUniform (const (uniform, 0)) 
            sides <- fmap makeSide <$> toPrimitiveStream riStream 
            let projectedSides = proj uni <$> sides
                filterMode = SamplerFilter Linear Linear Linear (Just 4)
                edge = (pure ClampToEdge, 1.0)
            samp <- newSampler2D (\ri -> (tex ri, filterMode, edge))

            uv <- rasterize (\ri -> (Front, ViewPort (V2 0 0) (riScreenSize ri), DepthRange 0 1) ) projectedSides
            let litFrags = light samp <$> uv
                litFragsWithDepth = withRasterizedInfo
                                       (\p x -> (p , (rasterizedFragCoord x)^._z)) litFrags
                colorOption = ContextColorOption (BlendRgbAlpha (FuncAdd, FuncAdd) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors One Zero) (V4 0 0 0 0)) (pure True)
                depthOption = DepthOption Less True
            
            drawContextColorDepth (const (colorOption, depthOption)) litFragsWithDepth
        boardShader :: Buffer os (Uniform UniInput) -> Shader os (ContextFormat FrameBufferColorFormat Depth) (RenderInput os) ()
        boardShader uniform = do
            uni <- getUniform (const (uniform, 0))
            boards <- fmap makeBoard <$> toPrimitiveStream riStream
            let projectedSides = proj uni <$> boards
                filterMode = SamplerFilter Linear Linear Linear (Just 4)
                edge = (pure ClampToEdge, 1.0)
            samp <- newSampler2D (\ri -> (tex ri, filterMode, edge))

            uv <- rasterize (\ri -> (Front, ViewPort (V2 0 0) (riScreenSize ri), DepthRange 0 1) ) projectedSides
            let litFrags = light samp <$> uv
                litFragsWithDepth = withRasterizedInfo
                                       (\p x -> (p, (rasterizedFragCoord x)^._z)) litFrags
                colorOption = ContextColorOption (BlendRgbAlpha (FuncAdd, FuncAdd) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors One Zero) (V4 0 0 0 0)) (pure True)
                depthOption = DepthOption Lequal True
            
            drawContextColorDepth (const (colorOption, depthOption)) litFragsWithDepth

loop (shader,shader') uniform camera = do  
  closeRequested <- GLFW.windowShouldClose   
  closeKeyPressed <- keyIsPressed GLFW.Key'Q
  (keyH,keyJ,keyK,keyL) <- (,,,) <$> keyIsPressed GLFW.Key'H
                                 <*> keyIsPressed GLFW.Key'J
                                 <*> keyIsPressed GLFW.Key'K
                                 <*> keyIsPressed GLFW.Key'L

  positions :: Buffer os (B3 Float) <- newBuffer 4  
  normals   :: Buffer os (B3 Float) <- newBuffer 6  
  writeBuffer positions 0 [V3 1 1 0, V3 1 (-1) 0, V3 (-1) 1 0, V3 (-1) (-1) 0]        
  writeBuffer normals 0 [V3 0 0 1]
  positions' :: Buffer os (B3 Float) <- newBuffer 4  
  normals'   :: Buffer os (B3 Float) <- newBuffer 6  
  writeBuffer positions' 0 [V3 1 0 1, V3 1 0 (-1), V3 (-1) 0 1, V3 (-1) 0 (-1)]        
  writeBuffer normals' 0 [V3 0 1 0]

  tex <- loadImage _front0_png
  btex <- loadImage _maptips_grass_png

  let makePrimitives p n = do   
        pArr <- newVertexArray p  
        nArr <- newVertexArray n  
        return $ toPrimitiveArrayInstanced TriangleStrip (,) pArr nArr   
 
  -- Write this frames uniform value   
  size <- getContextBuffersSize  
  let camera' = camera&_x+~(if 
                            | keyL -> 1
                            | keyH -> -1
                            | otherwise ->  0)
                      &_y+~(if 
                            | keyJ -> 1
                            | keyK -> -1
                            | otherwise ->  0)
      viewTarget = V3 0 0 0 
      viewUp = V3 0 1 0 
      normMat = identity
      uni = (fromIntegral <$> size, normMat, camera', viewTarget, viewUp) 
  writeBuffer uniform 0 [uni]  

  render $ do   
      clearContextColor (V4 0 0.25 1 0)
      clearContextDepth 1
      prims <- makePrimitives positions normals
      prims' <- makePrimitives positions' normals'
      shader' $ RenderInput size prims' btex
      shader  $ RenderInput size prims  tex
  swapContextBuffers  
 
  unless (closeRequested || closeKeyPressed) $  
      loop (shader,shader') uniform camera'

keyIsPressed k = (== GLFW.KeyState'Pressed) <$> GLFW.getKey k

-- light :: ColorSampleable c =>
--        Sampler2D (Format c) -> (t, V2 (S F Float)) -> ColorSample F c
light samp (normal, uv) =   
  sample2D samp SampleAuto (Just 1) Nothing uv 

makeSide :: Fractional a => (V3 a, V3 a) -> (V3 a, V3 a, V2 a)
makeSide (p, normal) = (p&_y+~1, normal, uv)    
  where uv = (p^._xy + 1) / 2

makeBoard :: Fractional a => (V3 a, V3 a) -> (V3 a, V3 a, V2 a)
makeBoard (p, normal) = (p, normal, uv)
  where uv = (p^._xz + 1) / 2

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
