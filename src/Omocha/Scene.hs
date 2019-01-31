{-# LANGUAGE StandaloneDeriving
 , DeriveDataTypeable
 , FlexibleContexts
 , RankNTypes 
 , ScopedTypeVariables 
 , RecordWildCards
 , GADTs
 #-}

module Omocha.Scene (
    UniInput
    , boardShader
    , monoShader
    , light
    , windowSize
    , modelNorm 
    , viewCamera
    , viewTarget
    , viewUp    
    , VBuffer
    , DrawVertex(..)
    , OmochaShaderType(..)
    , Scene(..)
    , Mesh(..)
    , TextureInput
    , PlainInput
    , RenderInput(..)
) where


import Omocha.Bitmap
import Data.Tree as Tree
import Data.Tree (Tree(), Forest)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Dynamic
import Data.Typeable
import Data.Int (Int32)
import qualified Data.Vector as V 
import qualified Linear.V as V
import qualified Data.Foldable as F
import Graphics.GPipe 
import Linear.Matrix
import Linear.Vector
import Linear.Metric
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Exception (MonadAsyncException)
import Control.Lens


data DrawVertex = DrawVertex {
                    dvPosition :: V3 Float,
                    dvNormal :: V3 Float,
                    dvUv :: V2 Float
                } deriving (Show)


data Mesh = Mesh {
              vertices :: [DrawVertex],
              indices :: Maybe [Int],
              offset :: V3 Float,
              textureImage :: Maybe Bitmap,
              shaderType :: OmochaShaderType
          }

instance Show Mesh where
    show Mesh{..} = show vertices ++ show indices ++ show shaderType

data OmochaShaderType = BoardShader | TargetBoard
    deriving (Eq, Ord, Show)


data Scene = Scene {
               meshes :: [Mesh],
               camera :: V3 Float
           } deriving (Show)


type VBuffer = (B3 Float, B3 Float, B2 Float)

data TextureInput
data PlainInput

data RenderInput os tag where
    RenderInput :: V2 Int -> PrimitiveArray Triangles VBuffer -> Texture2D os (Format RGBAFloat) -> RenderInput os TextureInput
    PlainInput :: V2 Int -> PrimitiveArray Triangles VBuffer -> RenderInput os PlainInput

riScreenSize :: RenderInput os tag -> V2 Int
riScreenSize (RenderInput vp _ _) = vp
riScreenSize (PlainInput vp _) = vp
riStream :: RenderInput os tag -> PrimitiveArray Triangles VBuffer
riStream (RenderInput _ st _) = st
riStream (PlainInput _ st) = st
tex :: RenderInput os TextureInput -> Texture2D os (Format RGBAFloat)
tex (RenderInput _ _ tex) = tex



type UniInput = (B2 Int32, V3 (B3 Float), B3 Float, B3 Float, B3 Float, B Float)

windowSize  (a, _, _, _, _, _) = a
modelNorm   (_, a, _, _, _, _) = a
viewCamera  (_, _, a, _, _, _) = a 
viewTarget  (_, _, _, a, _, _) = a
viewUp      (_, _, _, _, a, _) = a


boardShader :: ((UniformFormat UniInput V) 
                -> (V3 VFloat, V3 VFloat, V2 VFloat) 
                -> (V3 (S V Float), V3 VFloat, V2 (S V Float))
               )-> Window os RGBAFloat Depth 
                -> Buffer os (Uniform UniInput) 
                -> Shader os (RenderInput os TextureInput) ()
boardShader pick win uniform = do
    uni <- getUniform (const (uniform, 0))
    boards <- fmap (pick uni) <$> toPrimitiveStream riStream
    let projected = fmap (\(v, n, uv) -> let (v', n') = proj uni (v, n) in (v', (n', uv))) boards
        filterMode = SamplerFilter Linear Linear Linear (Just 16)
        edge = (pure ClampToEdge, 1.0)
    samp <- newSampler2D $ \ri -> (tex ri, filterMode, edge)

    fragmentStream <- rasterize (\ri -> (Front, ViewPort (V2 0 0) (riScreenSize ri), DepthRange 0 1) ) projected
    let litFrags = light samp <$> fragmentStream
        litFragsWithDepth = withRasterizedInfo
                               (\p x -> (p, (rasterizedFragCoord x)^._z)) litFrags
        colorOption = ContextColorOption (BlendRgbAlpha (FuncAdd, FuncAdd) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors Zero One) (V4 0 0 0 0)) (pure True)
        depthOption = DepthOption Lequal True
    
    drawWindowColorDepth (const (win, colorOption, depthOption)) litFragsWithDepth

monoShader :: ((UniformFormat UniInput V) 
               -> (V3 VFloat, V3 VFloat, V2 VFloat) 
               -> (V3 (S V Float), V3 VFloat, V2 (S V Float))
              )-> Window os RGBAFloat Depth 
               -> Buffer os (Uniform UniInput) 
               -> Shader os (RenderInput os PlainInput) ()
monoShader pick win uniform = do
    uni <- getUniform (const (uniform, 0))
    boards <- fmap (pick uni) <$> toPrimitiveStream riStream
    let projected = fmap (\(v, n, _) -> let (v', n') = proj uni (v, n) in (v',  V4 1 0 0 (0.5) :: V4 VFloat)) boards
    fragmentStream <- rasterize (\ri -> (Front, ViewPort (V2 0 0) (riScreenSize ri), DepthRange 0 1) ) projected
    let litFrags = fragmentStream
        litFragsWithDepth = withRasterizedInfo
                               (\p x -> (p, (rasterizedFragCoord x)^._z)) litFrags
    let colorOption = ContextColorOption (BlendRgbAlpha (FuncAdd, FuncAdd) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors Zero One) (V4 0 0 0 0)) (pure True)
        depthOption = DepthOption Lequal True
    
    drawWindowColorDepth (const (win, colorOption, depthOption)) litFragsWithDepth


light :: ColorSampleable c => Sampler2D (Format c) -> (t, V2 (S F Float)) -> ColorSample F c
light samp (normal, uv) = sample2D samp SampleAuto (Just 1) Nothing uv 


proj :: (Functor f, Floating (ConvertFloat a), Convert a, Foldable r, Additive r) =>
        (V2 a, f (r VFloat), V3 (ConvertFloat a), V3 (ConvertFloat a), V3 (ConvertFloat a), t)
        -> (V3 (ConvertFloat a), r VFloat)
        -> (V4 (ConvertFloat a), f FlatVFloat)
proj uni (V3 px py pz, normal) =   
    let modelViewProj = perspective (pi/3) (let V2 w h = windowSize uni in (toFloat w) / (toFloat h)) 1 (-1)
        normMat = modelNorm uni
        viewProj = lookAt' (viewCamera uni) (viewTarget uni) (viewUp uni)
    in (modelViewProj !*! viewProj !* V4 px py pz 1, (fmap Flat $ normMat !* normal))   

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
     
