{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Astrata.Shader where

import Astrata.Scene ()
import Astrata.Shape
import Astrata.Uniform
import Control.Monad.Exception (MonadException)
import Data.Tuple.Extra (both)
import Data.Vector qualified as V
import Graphics.GPipe hiding (trace, transpose)
import RIO hiding (trace, (<*))

type BPosition = B3 Float

type BScreenPosition = B2 Float

type BNormal = B3 Float

type BUV = B2 Float

data ShaderEnvironment os = ShaderEnvironment
  { size :: V2 Int,
    primitives :: PrimitiveArray Triangles (BPosition, BNormal, BUV),
    colorImage :: Texture2D os (Format RGBAFloat),
    obj :: ObjectId
  }

data PlainEnvironment os = PlainEnvironment
  { size :: V2 Int,
    color :: V4 Float,
    obj :: ObjectId,
    primitives :: PrimitiveArray Triangles (BPosition, BNormal)
  }

data TextEnvironment os = TextEnvironment
  { size :: V2 Int,
    primitives :: PrimitiveArray Triangles (BScreenPosition, BUV),
    image :: Texture2D os (Format RGBAFloat)
  }

boardShader ::
  Window os RGBAFloat Depth ->
  ApplicationUniforms os ->
  Shader os (ShaderEnvironment os) ()
boardShader win unis = do
  g <- readGlobalUniform unis
  o <- readObjectUniform'' unis
  boards <- toPrimitiveStream (.primitives)
  let projected = (\(v, n, uv) -> (proj g (normalizePoint (o.proj !* point v) + o.position), (g.modelNorm !* n, uv, biasMat !* lightProj g v))) <$> boards
      filterMode = SamplerFilter Linear Linear Linear (Just 16)
      edge = (pure ClampToEdge, 1.0)
  samp <- newSampler2D $ \env -> (env.colorImage, filterMode, edge)

  fragmentStream <- rasterize (\env -> (Front, ViewPort (V2 0 0) env.size, DepthRange 0 1)) projected
  let litFrags =
        (\(_c, u, _s) -> light samp u)
          <$> fragmentStream
      frags = filterFragments (\f -> f ^. _w /=* 0) litFrags
      fragsWithDepth =
        withRasterizedInfo
          (\p x -> (p, rasterizedFragCoord x ^. _z))
          frags
  let colorOption = ContextColorOption (BlendRgbAlpha (FuncAdd, Min) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors SrcAlpha DstAlpha) (V4 0 0 0 0)) (pure True)
      depthOption = DepthOption Lequal True
  drawWindowColorDepth (const (win, colorOption, depthOption)) fragsWithDepth

biasMat :: (Fractional a) => M44 a
biasMat =
  V4
    (V4 0.5 0 0 0.5)
    (V4 0 0.5 0 0.5)
    (V4 0 0 0.5 0.5)
    (V4 0 0 0 1)

normalizeS :: (BooleanOf (f a) ~ BooleanOf a, Metric f, IfB (f a), EqB a, Floating a) => f a -> f a
normalizeS v = ifB (l ==* 1 ||* l ==* 0) v (fmap (/ sqrt l) v)
  where
    l = quadrance v

readObjectUniform'' :: ApplicationUniforms os -> Shader os (ShaderEnvironment os) (ObjectUniformS a)
readObjectUniform'' unis = do
  uni <- getUniform (\env -> let (ObjectId id) = env.obj in (unis.objects, id))
  let (position, modelProj) = uni
  return $ ObjectUniform position modelProj

readObjectUniform' :: ApplicationUniforms os -> Shader os (PlainEnvironment os) (ObjectUniformS a)
readObjectUniform' unis = do
  uni <- getUniform (\env -> let (ObjectId id) = env.obj in (unis.objects, id))
  let (position, modelProj) = uni
  return $ ObjectUniform position modelProj

phongShader ::
  Window os RGBAFloat Depth ->
  ApplicationUniforms os ->
  Shader os (PlainEnvironment os) ()
phongShader win unis = do
  g <- readGlobalUniform unis
  o <- readObjectUniform' unis
  boards <- toPrimitiveStream $ \env -> (\v -> (v, env.color)) <$> env.primitives
  let projected = (\((v, n), c) -> let v' = normalizePoint (o.proj !* point v) + o.position in (proj g v', (g.modelNorm !* n, c, biasMat !* (point . normalizePoint $ lightProj g v')))) <$> boards
  fragmentStream <- rasterize (\env -> (Front, ViewPort (V2 0 0) env.size, DepthRange 0 1)) projected
  unif <- readGlobalUniform unis
  let lightDir :: V3 FFloat = normalizeS unif.lightDirection
  let litFrags =
        withRasterizedInfo
          ( \(n, c, _s) x ->
              let v = rasterizedFragCoord x
                  diffuse = point . pure $ clamp (lightDir `dot` n * 10) 0.1 1.0
               in (c * diffuse, v ^. _z)
          )
          fragmentStream
  let colorOption = ContextColorOption (BlendRgbAlpha (FuncAdd, Min) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors SrcAlpha DstAlpha) (V4 0 0 0 0)) (pure True)
      depthOption = DepthOption Less True
  drawWindowColorDepth (const (win, colorOption, depthOption)) litFrags

monoShader :: Window os RGBAFloat Depth -> ApplicationUniforms os -> Shader os (PlainEnvironment os) ()
monoShader = phongShader

light :: (ColorSampleable c) => Sampler2D (Format c) -> V2 FFloat -> ColorSample F c
light samp = sample2D samp (SampleLod 0) (Just 1) Nothing

proj :: (Floating (ConvertFloat a), Convert a) => GlobalUniform a (ConvertFloat a) -> V3 (ConvertFloat a) -> V4 (ConvertFloat a)
proj g v = viewProjection g !* point v

lightProj :: (Floating (ConvertFloat a), Convert a) => GlobalUniform a (ConvertFloat a) -> V3 (ConvertFloat a) -> V4 (ConvertFloat a)
lightProj _g v = infinitePerspective (pi / 3) 1 0.01 !*! lookAt' (V3 60 40 80) (V3 60 0 0) (V3 0 1 0) !* point v

projection :: (Floating (ConvertFloat a), Convert a) => GlobalUniform a (ConvertFloat a) -> M44 (ConvertFloat a)
projection uni = infinitePerspective (pi / 3) (let V2 w h = uni.windowSize in toFloat w / toFloat h) 0.01

viewProjection :: (Floating (ConvertFloat a), Convert a) => GlobalUniform a (ConvertFloat a) -> M44 (ConvertFloat a)
viewProjection g =
  let p = projection g
      v = lookAt' g.viewCamera g.viewTarget g.viewUp
   in p !*! v

lookAt' :: (Floating a) => V3 a -> V3 a -> V3 a -> M44 a
lookAt' eye center up =
  V4
    (V4 (xa ^. _x) (xa ^. _y) (xa ^. _z) xd)
    (V4 (ya ^. _x) (ya ^. _y) (ya ^. _z) yd)
    (V4 (-(za ^. _x)) (-(za ^. _y)) (-(za ^. _z)) zd)
    (V4 0 0 0 1)
  where
    za = signorm $ center - eye
    xa = signorm $ cross za up
    ya = cross xa za
    xd = -dot xa eye
    yd = -dot ya eye
    zd = dot za eye

textShader ::
  Window os RGBAFloat Depth ->
  ApplicationUniforms os ->
  Shader os (TextEnvironment os) ()
textShader win unis = do
  u <- readGlobalUniform unis
  let ws = toFloat <$> (u.windowSize :: V2 (S V Int))
      wh = ws / 2
      edge = (pure ClampToEdge, 1.0)
  samp :: Sampler2D (Format RGBAFloat) <- newSampler2D $ \env -> (env.image, SamplerFilter Linear Linear Linear (Just 16), edge)
  prims :: PrimitiveStream p (V2 VFloat, V2 VFloat) <- toPrimitiveStream (\env -> env.primitives)
  let primitiveStream :: PrimitiveStream p (VPos, (V4 VFloat, V2 VFloat)) = fmap (\(v, uv) -> let v' = (v - wh) / wh in (V4 (v' ^. _x) (-(v' ^. _y)) 0 1, (V4 1 1 0 1, uv))) prims
  fragmentStream :: FragmentStream (V4 FFloat, V2 FFloat) <- rasterize (\(TextEnvironment ri _ _) -> (Front, ViewPort (V2 0 0) ri, DepthRange 0 1)) primitiveStream
  let litFrags :: FragmentStream (FragColor RGBAFloat) = light samp . snd <$> fragmentStream
      colorOption = ContextColorOption (BlendRgbAlpha (FuncAdd, FuncAdd) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors Zero One) (V4 0 0 0 0)) (pure True)
  drawWindowColor (const (win, colorOption)) litFrags

data HighlightEnvironment os = HighlightEnvironment
  { size :: V2 Int,
    color :: V4 Float,
    primitives :: PrimitiveArray Triangles (BPosition, BNormal)
  }

highlightShader ::
  Window os RGBAFloat Depth ->
  ApplicationUniforms os ->
  Shader os (HighlightEnvironment os) ()
highlightShader win unis = do
  g <- readGlobalUniform unis
  uni <- getUniform (\_ -> (unis.highlight, 0))
  let (position, modelProj) = uni
  boards <- toPrimitiveStream $ \env -> (\v -> (v, env.color)) <$> env.primitives
  let projected = (\((v, n), c) -> let v' = normalizePoint (modelProj !* point v) + position in (proj g v', (g.modelNorm !* n, c, biasMat !* (point . normalizePoint $ lightProj g v')))) <$> boards
  fragmentStream <- rasterize (\env -> (Front, ViewPort (V2 0 0) env.size, DepthRange 0 1)) projected
  unif <- readGlobalUniform unis
  let lightDir :: V3 FFloat = normalizeS unif.lightDirection
  let litFrags =
        withRasterizedInfo
          ( \(n, c, _s) x ->
              let v = rasterizedFragCoord x
                  diffuse = point . pure $ clamp (lightDir `dot` n * 10) 0.1 1.0
               in (c * diffuse, v ^. _z)
          )
          fragmentStream
  let colorOption = ContextColorOption (BlendRgbAlpha (FuncAdd, Min) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors SrcAlpha DstAlpha) (V4 0 0 0 0)) (pure True)
      depthOption = DepthOption Less True
  drawWindowColorDepth (const (win, colorOption, depthOption)) litFrags

data GridInput = GridInput (V2 Int) (PrimitiveArray Lines (B3 Float))

gridShader ::
  (HasCallStack, ContextHandler ctx, MonadIO m, Control.Monad.Exception.MonadException m) =>
  (Int, Int) ->
  V2 Float ->
  V3 Float ->
  Window os RGBAFloat Depth ->
  ApplicationUniforms os ->
  ContextT ctx os m (CompiledShader os (V2 Int))
gridShader size unit offset win unis =
  do
    let gridColor :: V4 Float = V4 0.75 0.75 0.75 1
        (sx, sy) = size
        dts = interval 4 0 1
        xss = V.generate (sy + 1) $ \y -> do
          x :: Float <- V.enumFromN 0 sx
          dt <- dts
          return (V2 ((x + dt) * (unit ^. _x)) (fromIntegral y * (unit ^. _y)))
        yss = V.generate (sx + 1) $ \x -> do
          y :: Float <- V.enumFromN 0 sy
          dt <- dts
          return (V2 (fromIntegral x * (unit ^. _x)) ((y + dt) * (unit ^. _y)))
        v3 (V2 x y) = V3 x 0 y

    let v =
          V.toList
            $ V.concatMap
              ( \xs ->
                  V.concatMap
                    ( \pair ->
                        let (start, end) = both (+ offset ^. _xz) pair
                         in V.singleton (v3 start) `V.snoc` v3 end
                    )
                    (V.zip xs (V.tail xs))
              )
              xss
            V.++ V.concatMap
              ( \ys ->
                  V.concatMap
                    ( \pair ->
                        let (start, end) = both (+ offset ^. _xz) pair
                         in V.singleton (v3 start) `V.snoc` v3 end
                    )
                    (V.zip ys (V.tail ys))
              )
              yss
    vbuf :: Buffer os (B3 Float) <- newBuffer (length v)
    unless (null v) $ writeBuffer vbuf 0 v
    s <- compileShader $ do
      uni <- readGlobalUniform unis
      prims <- toPrimitiveStream $ \(GridInput _ st) -> (\v' -> (v', gridColor)) <$> st
      let projected = first (proj uni) <$> prims
      fragmentStream <- rasterize (\(GridInput vp _) -> (Front, ViewPort (V2 0 0) vp, DepthRange 0 1)) projected
      let colorOption = ContextColorOption (BlendRgbAlpha (FuncAdd, FuncAdd) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors Zero One) (V4 0 0 0 0)) (pure True)
      drawWindowColor (const (win, colorOption)) fragmentStream
    return $ \vpSize -> do
      pArr <- newVertexArray vbuf
      let pa = toPrimitiveArray LineList pArr
      s $ GridInput vpSize pa

bundle :: (Foldable t, Monad m) => t (b -> m ()) -> b -> m ()
bundle fs b = mapM_ (\f -> f b) fs

transpose :: (HasCallStack) => Vector (Vector a) -> Vector (Vector a)
transpose v = case V.uncons v of
  Nothing -> V.empty
  Just (x, xss) -> case V.uncons x of
    Nothing -> transpose xss
    Just (x', xs) ->
      let (hds, tls) = V.unzip $ V.map (\x'' -> (V.head x'', V.tail x'')) xss
       in (x' `V.cons` hds) `V.cons` transpose (xs `V.cons` tls)

rankBundle' :: Vector (Vector a) -> Vector a
rankBundle' fs = let rs = (join . transpose $ fs) in rs
