{-# LANGUAGE ScopedTypeVariables
           , TypeFamilies
           , TemplateHaskell
           , QuasiQuotes
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

main = do 
    runContextT GLFW.newContext (ContextFormatColorDepth SRGB8 Depth16) $ do  
        positions :: Buffer os (B2 Float) <- newBuffer 4  
        normals   :: Buffer os (B3 Float) <- newBuffer 6  
        tangents  :: Buffer os (B3 Float) <- newBuffer 6  
        writeBuffer positions 0 [V2 1 1, V2 1 (-1), V2 (-1) 1, V2 (-1) (-1)]        
        writeBuffer normals 0 [V3 0 0 1]
        writeBuffer tangents 0 [V3 (1) 0 0]

        -- Create a buffer for the uniform values        
        uniform :: Buffer os (Uniform (V4 (B4 Float), V3 (B3 Float))) <- newBuffer 1  
    
        -- Make a Render action that returns a PrimitiveArray for the cube   
        let makePrimitives = do   
              pArr <- newVertexArray positions  
              nArr <- newVertexArray normals  
              tArr <- newVertexArray tangents  
              let sideInstances = zipVertices (,) nArr tArr          
              return $ toPrimitiveArrayInstanced TriangleStrip (,) pArr sideInstances   
      
        let (size,img) = getImage _front0_png
            edge = (pure ClampToEdge, 1.0)  
            rasterOptions = (FrontAndBack, ViewPort 0 800, DepthRange 0 1)
        tex <- newTexture2D RGBA8 size maxBound

        writeTexture2D tex 0 0 size $ P.pixelFold getJuicyPixel [] img 

        shader <- compileShader $ do  
                      sides <- fmap makeSide <$> toPrimitiveStream id
                      (modelViewProj, normMat) <- getUniform (const (uniform, 0))  
                      let filterMode = SamplerFilter Linear Linear Linear (Just 4)  
                          projectedSides = proj modelViewProj normMat <$> sides            
                      samp <- newSampler2D (const (tex, filterMode, edge))

                      uv <- rasterize (\_ -> rasterOptions) projectedSides
                      let litFrags = light samp <$> uv  
                          litFragsWithDepth = withRasterizedInfo   
                              (\(V4 r g b a) x -> (V3 r g b, (rasterizedFragCoord x)^._z)) litFrags  
                          colorOption = ContextColorOption NoBlending (pure True)  
                          depthOption = DepthOption Less True                            
    
                      drawContextColorDepth (const (colorOption, depthOption)) litFragsWithDepth  
       
        loop makePrimitives shader uniform 0   



getJuicyPixel xs _x _y pix =  
  let P.PixelRGBA8 r g b a = P.convertPixel pix in V4 r g b a : xs   

loop makePrimitives shader uniform angle = do  
  -- Write this frames uniform value   
  size@(V2 w h) <- getContextBuffersSize  
  let modelRot = fromQuaternion (axisAngle (V3 1 1 1) angle)  
      -- modelMat = mkTransformationMat modelRot (pure 0)  
      projMat = perspective (pi/3) (fromIntegral w / fromIntegral h) 1 100   
      viewMat = mkTransformationMat identity (- V3 0 0 5)  
      viewProjMat = projMat !*! viewMat 
      normMat = modelRot  
  writeBuffer uniform 0 [(viewProjMat, normMat)]  

  render $ do   
      clearContextColor (V3 1 1 1)
      clearContextDepth 1
      prims <- makePrimitives
      shader prims   
  swapContextBuffers  

  closeRequested <- GLFW.windowShouldClose   
  unless closeRequested $  
      loop makePrimitives shader uniform (0)


-- light :: ColorSampleable c =>
--        Sampler2D (Format c) -> (t, V2 (S F Float)) -> ColorSample F c
light samp (normal, uv) =   
  sample2D samp SampleAuto Nothing Nothing uv * pure (normal `dot` V3 0 0 1)     


-- Project the sides coordinates using the instance's normal and tangent  
makeSide :: Fractional a => (V2 a, (V3 a, V3 a)) -> (V3 a, V3 a, V2 a)
makeSide (p@(V2 x y), (normal, tangent)) =   
  (V3 x y 1 *! V3 tangent bitangent normal, normal, uv)    
  where bitangent = cross normal tangent  
        uv = (p + 1) / 2  

proj
  :: (Functor m, Functor f, Num a, Foldable r, Additive r) =>
       m (V4 a) -> f (r VFloat) -> (V3 a, r VFloat, t) -> (m a, (f FlatVFloat, t))
-- Project the cube's positions and normals with ModelViewProjection matrix  
proj modelViewProj normMat (V3 px py pz, normal, uv) =   
  (modelViewProj !* V4 px py pz 1, (fmap Flat $ normMat !* normal, uv))   
