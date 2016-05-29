{-# LANGUAGE ScopedTypeVariables
           , TypeFamilies
           , TemplateHaskell
           , QuasiQuotes
           , MultiWayIf
           , FlexibleContexts #-}   

module Main where   

import Omocha.Utils

import Graphics.GPipe   
import qualified Graphics.GPipe.Context.GLFW as GLFW  
import Control.Monad (unless)  
import Paths_omocha

import qualified Codec.Picture as P
import qualified Codec.Picture.Types as P

import Control.Lens

loadBitmapsWith [|getDataFileName|] "static/images"

getImage :: Bitmap -> (V2 Int, P.Image P.PixelRGBA8)
getImage bmp = (siz, img)
    where img = bitmapImage bmp
          siz = V2 (P.imageWidth img) (P.imageHeight img)

type UniInput = (V4 (B4 Float), V3 (B3 Float), B3 Float, B3 Float, B3 Float)

viewModel  (a, _, _, _, _) = a 
modelNorm  (_, a, _, _, _) = a
viewOrigin (_, _, a, _, _) = a
viewTarget (_, _, _, a, _) = a
viewUp     (_, _, _, _, a) = a

main = do 
    runContextT GLFW.newContext (ContextFormatColorDepth SRGB8 Depth16) $ do  
        positions :: Buffer os (B2 Float) <- newBuffer 4  
        normals   :: Buffer os (B3 Float) <- newBuffer 6  
        writeBuffer positions 0 [V2 1 1, V2 1 (-1), V2 (-1) 1, V2 (-1) (-1)]        
        writeBuffer normals 0 [V3 0 0 1]
        uniform :: Buffer os (Uniform UniInput) <- newBuffer 1  
    
        let makePrimitives = do   
              pArr <- newVertexArray positions  
              nArr <- newVertexArray normals  
              return $ toPrimitiveArrayInstanced TriangleStrip (,) pArr nArr   
     
        let (size,img) = getImage _front0_png

        tex <- newTexture2D RGBA8 size maxBound
        writeTexture2D tex 0 0 size $ P.pixelFold getJuicyPixel [] img 

        let pictureShader = do
              sides <- fmap makeSide <$> toPrimitiveStream id 
              uni <- getUniform (const (uniform, 0)) 
              let projectedSides = proj uni <$> sides
                  rasterOptions = (Front, ViewPort 0 800, DepthRange 0 1)
                  filterMode = SamplerFilter Linear Linear Linear (Just 4)
                  edge = (pure ClampToEdge, 1.0)
              samp <- newSampler2D (const (tex, filterMode, edge))

              uv <- rasterize (\_ -> rasterOptions) projectedSides
              let litFrags = light samp <$> uv
                  litFragsWithDepth = withRasterizedInfo
                                         (\(V4 r g b a) x -> (V3 r g b, (rasterizedFragCoord x)^._z)) litFrags
                  colorOption = ContextColorOption NoBlending (pure True)
                  depthOption = DepthOption Less True
              
              drawContextColorDepth (const (colorOption, depthOption)) litFragsWithDepth

        shader <- compileShader pictureShader
        loop makePrimitives shader uniform (V3 0 0 (-1))

getJuicyPixel xs _ _ pix =  
  let P.PixelRGBA8 r g b a = P.convertPixel pix in V4 r g b a : xs   

loop makePrimitives shader uniform camera = do  
  closeRequested <- GLFW.windowShouldClose   
  closeKeyPressed <- keyIsPressed GLFW.Key'Q
  (keyH,keyJ,keyK,keyL) <- (,,,) <$> keyIsPressed GLFW.Key'H
                     <*> keyIsPressed GLFW.Key'J
                     <*> keyIsPressed GLFW.Key'K
                     <*> keyIsPressed GLFW.Key'L
 
  -- Write this frames uniform value   
  size@(V2 w h) <- getContextBuffersSize  
  let projMat = perspective (pi/3) (fromIntegral $ w `div` h) 1 100   
      -- viewMat = mkTransformationMat identity (V3 0 0 5)  
      camera' = camera&_x+~(if 
                            | keyL -> 0.01 
                            | keyH -> -0.01
                            | otherwise ->  0)
                      &_y+~(if 
                            | keyJ -> 0.01 
                            | keyK -> -0.01
                            | otherwise ->  0)
      viewTarget = V3 0 0 1 
      viewProjMat = projMat
      viewUp = V3 0 1 0 
      normMat = fromQuaternion (axisAngle (V3 1 1 1) 0)  
      uni = (viewProjMat, normMat, camera', viewTarget, viewUp) 
  writeBuffer uniform 0 [uni]  

  render $ do   
      clearContextColor (V3 1 1 1)
      clearContextDepth 1
      prims <- makePrimitives
      shader prims   
  swapContextBuffers  

 
  unless (closeRequested || closeKeyPressed) $  
      loop makePrimitives shader uniform camera'


keyIsPressed k = (== GLFW.KeyState'Pressed) <$> GLFW.getKey k

-- nil :: (Num a, Additive f) => Bool -> f a -> f a
-- nil p n = if 
--           | p -> 0.005
--           | n -> (-0.005)
--           | otherwise -> 0


-- light :: ColorSampleable c =>
--        Sampler2D (Format c) -> (t, V2 (S F Float)) -> ColorSample F c
light samp (normal, uv) =   
  sample2D samp SampleAuto Nothing Nothing uv -- * pure (normal `dot` V3 0 0 1)     


-- Project the sides coordinates using the instance's normal and tangent  
makeSide :: Fractional a => (V2 a, V3 a) -> (V3 a, V3 a, V2 a)
makeSide (p@(V2 x y), normal) = (V3 x y 1, normal, uv)    
  where uv = (p + 1) / 2  

proj :: (Floating a, Functor m, Functor f, Foldable r, Additive r) =>
    (m (V4 a), f (r VFloat), V3 a, V3 a, V3 a)
      -> (V3 a, r VFloat, t) -> (m a, (f FlatVFloat, t))
proj uni (V3 px py pz, normal, uv) =   
  (modelViewProj !*! vv !* V4 px py pz 1  , (fmap Flat $ normMat !* normal, uv))   
  where vv = lookAt' (viewOrigin uni) (viewTarget uni) (viewUp uni)
        modelViewProj = viewModel uni 
        normMat = modelNorm uni
        
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
