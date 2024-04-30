{-# LANGUAGE GADTs #-}

module Omocha.Shader where

import Data.Bits (complement)
import Graphics.GPipe
import Omocha.Context
import Omocha.Uniform
import RIO

type BPosition = B3 Float

type BScreenPosition = B2 Float

type BNormal = B3 Float

type BUV = B2 Float

data RenderInput os = RenderInput (V2 Int) (PrimitiveArray Triangles (BPosition, BNormal, BUV)) (Texture2D os (Format RGBAFloat))

data PlainInput os = PlainInput (V2 Int) (V4 Float) (PrimitiveArray Triangles (BPosition, BNormal))

data TextInput os = TextInput (V2 Int) (PrimitiveArray Triangles (BScreenPosition, BUV)) (Texture2D os (Format RGBAFloat))

boardStencilShader ::
  Window os RGBAFloat DepthStencil ->
  ApplicationUniforms os ->
  Shader os (RenderInput os) ()
boardStencilShader win unis = do
  uni <- readGlobalUniform unis
  boards <- toPrimitiveStream (\(RenderInput _ st _) -> st)
  let projected = (\(v, n, _) -> let (v', n') = proj uni (v, n) in let v'' = v' ^. _xyz * (n' * 0.1) in (v', ())) <$> boards

  frags <- rasterize (\(RenderInput ri _ _) -> (Front, ViewPort (V2 0 0) ri, DepthRange 0 1)) projected
  let stencilOption = StencilOption Always 1 OpKeep OpReplace (complement 0) (complement 0)
  drawWindowStencil (const (win, FrontBack stencilOption stencilOption)) frags

boardShader ::
  Window os RGBAFloat DepthStencil ->
  ApplicationUniforms os ->
  Shader os (RenderInput os) ()
boardShader win unis = do
  g <- readGlobalUniform unis
  o <- readObjectUniform unis
  boards <- toPrimitiveStream (\(RenderInput _ ri _) -> ri)
  let projected = (\(v, n, uv) -> let (v', n') = proj g (v + o.position, n) in (v', (n', uv))) <$> boards
      filterMode = SamplerFilter Linear Linear Linear (Just 16)
      edge = (pure ClampToEdge, 1.0)
  samp <- newSampler2D $ \(RenderInput _ _ tex) -> (tex, filterMode, edge)

  fragmentStream <- rasterize (\(RenderInput ri _ _) -> (Front, ViewPort (V2 0 0) ri, DepthRange 0 1)) projected
  let litFrags = light samp . snd <$> fragmentStream
      frags = filterFragments (\f -> f ^. _w /=* 0) litFrags
      fragsWithDepth =
        withRasterizedInfo
          (\p x -> (p, rasterizedFragCoord x ^. _z))
          frags
  let colorOption = ContextColorOption (BlendRgbAlpha (FuncAdd, Min) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors SrcAlpha DstAlpha) (V4 0 0 0 0)) (pure True)
      depthOption = DepthOption Lequal True
      stencilOptions = FrontBack (StencilOption Equal 0 OpKeep OpKeep (complement 0) (complement 0)) (StencilOption Equal 0 OpKeep OpKeep (complement 0) (complement 0))
  drawWindowColorDepthStencil (const (win, colorOption, DepthStencilOption stencilOptions depthOption (FrontBack OpKeep OpKeep))) fragsWithDepth

monoStencil ::
  Window os RGBAFloat DepthStencil ->
  ApplicationUniforms os ->
  Shader os (PlainInput os) ()
monoStencil win unis = do
  uni <- readGlobalUniform unis
  boards <- toPrimitiveStream $ \(PlainInput _ c st) -> (\v -> (v, c)) <$> st
  let projected = (\((v, n), c) -> let (v', n') = proj uni (v, n) in let v'' = v' ^. _xyz + (n' * 0.1) in (v', V4 1 0 1 1 :: V4 VFloat)) <$> boards
  frags <- rasterize (\(PlainInput ri _ _) -> (Front, ViewPort (V2 0 0) ri, DepthRange 0 1)) projected
  let stencilOption = StencilOption Always 1 OpKeep OpReplace (complement 0) (complement 0)
  let colorOption = ContextColorOption NoBlending (pure True)
  -- depthOption = DepthOption Lequal True
  -- drawWindowStencil (const (win, FrontBack stencilOption stencilOption)) frags
  drawWindowColorStencil (const (win, colorOption, FrontBack stencilOption stencilOption)) frags

normalizeS :: (BooleanOf (f a) ~ BooleanOf a, Metric f, IfB (f a), EqB a, Floating a) => f a -> f a
normalizeS v = ifB (l ==* 1 ||* l ==* 0) v (fmap (/ sqrt l) v)
  where
    l = quadrance v

gouraudShader ::
  Window os RGBAFloat DepthStencil ->
  ApplicationUniforms os ->
  Shader os (PlainInput os) ()
gouraudShader win unis = do
  g <- readGlobalUniform unis
  o <- readObjectUniform unis
  boards <- toPrimitiveStream $ \(PlainInput _ c st) -> (\v -> (v, c)) <$> st
  let lightDir :: V3 VFloat = normalizeS g.lightDirection
  let projected = (\((v, n), c :: V4 VFloat) -> let (v', _) = proj g (v + o.position, n) in (v', let diffuse :: V4 VFloat = point . return $ clamp (lightDir `dot` n) 0.1 1.0 in c * diffuse)) <$> boards
  fragmentStream <- rasterize (\(PlainInput ri _ _) -> (Front, ViewPort (V2 0 0) ri, DepthRange 0 1)) projected
  let litFrags =
        withRasterizedInfo
          (\p x -> (p, rasterizedFragCoord x ^. _z))
          fragmentStream
  let colorOption = ContextColorOption (BlendRgbAlpha (FuncAdd, Min) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors SrcAlpha DstAlpha) (V4 0 0 0 0)) (pure True)
      depthOption = DepthOption Lequal True
      stencilOptions = FrontBack (StencilOption Equal 0 OpKeep OpKeep (complement 0) (complement 0)) (StencilOption Equal 0 OpKeep OpKeep (complement 0) (complement 0))
  drawWindowColorDepthStencil (const (win, colorOption, DepthStencilOption stencilOptions depthOption (FrontBack OpKeep OpKeep))) litFrags

phongShader ::
  Window os RGBAFloat DepthStencil ->
  ApplicationUniforms os ->
  Shader os (PlainInput os) ()
phongShader win unis = do
  g <- readGlobalUniform unis
  o <- readObjectUniform unis
  boards <- toPrimitiveStream $ \(PlainInput _ c st) -> (\v -> (v, c)) <$> st
  let projected = (\((v, n), c) -> let (v', n') = proj g (v + o.position, n) in (v', (n', c))) <$> boards
  fragmentStream <- rasterize (\(PlainInput ri _ _) -> (Front, ViewPort (V2 0 0) ri, DepthRange 0 1)) projected
  unif <- readGlobalUniform unis
  let lightDir :: V3 FFloat = normalizeS unif.lightDirection
  -- eyeDir = normalizeS $ unif.viewCamera
  let litFrags =
        withRasterizedInfo
          ( \(n, c) x ->
              let v = rasterizedFragCoord x
                  diffuse = point . return $ clamp (lightDir `dot` n) 0.1 1.0
               in -- halfLE :: V3 FFloat = normalizeS $ lightDir + eyeDir
                  -- specular :: FFloat = clamp (n `dot` halfLE) 0.0 1.0 ^* 50
                  (c * diffuse, v ^. _z)
          )
          fragmentStream
  let colorOption = ContextColorOption (BlendRgbAlpha (FuncAdd, Min) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors SrcAlpha DstAlpha) (V4 0 0 0 0)) (pure True)
      depthOption = DepthOption Lequal True
      stencilOptions = FrontBack (StencilOption Equal 0 OpKeep OpKeep (complement 0) (complement 0)) (StencilOption Equal 0 OpKeep OpKeep (complement 0) (complement 0))
  drawWindowColorDepthStencil (const (win, colorOption, DepthStencilOption stencilOptions depthOption (FrontBack OpKeep OpKeep))) litFrags

monoShader :: Window os RGBAFloat DepthStencil -> ApplicationUniforms os -> Shader os (PlainInput os) ()
monoShader = phongShader

monoShader' ::
  Window os RGBAFloat DepthStencil ->
  ApplicationUniforms os ->
  Shader os (PlainInput os) ()
monoShader' win unis = do
  g <- readGlobalUniform unis
  o <- readObjectUniform unis
  boards <- toPrimitiveStream $ \(PlainInput _ c st) -> (\v -> (v, c)) <$> st
  let projected = (\((v, n), c) -> let (v', _) = proj g (v + o.position, n) in (v', c)) <$> boards
  fragmentStream <- rasterize (\(PlainInput ri _ _) -> (Front, ViewPort (V2 0 0) ri, DepthRange 0 1)) projected
  let litFrags =
        withRasterizedInfo
          (\p x -> (p, rasterizedFragCoord x ^. _z))
          fragmentStream
  let colorOption = ContextColorOption (BlendRgbAlpha (FuncAdd, Min) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors SrcAlpha DstAlpha) (V4 0 0 0 0)) (pure True)
      depthOption = DepthOption Lequal True
      stencilOptions = FrontBack (StencilOption Equal 0 OpKeep OpKeep (complement 0) (complement 0)) (StencilOption Equal 0 OpKeep OpKeep (complement 0) (complement 0))
  drawWindowColorDepthStencil (const (win, colorOption, DepthStencilOption stencilOptions depthOption (FrontBack OpKeep OpKeep))) litFrags

light :: (ColorSampleable c) => Sampler2D (Format c) -> V2 FFloat -> ColorSample F c
light samp = sample2D samp SampleAuto (Just 1) Nothing

proj :: (Floating (ConvertFloat a), Convert a) => GlobalUniform a (ConvertFloat a) -> (V3 (ConvertFloat a), V3 (ConvertFloat a)) -> (V4 (ConvertFloat a), V3 (ConvertFloat a))
proj uni (v, normal) = (viewProjection uni !* point v, uni.modelNorm !* normal)

viewProjection :: (Floating (ConvertFloat a), Convert a) => GlobalUniform a (ConvertFloat a) -> M44 (ConvertFloat a)
viewProjection uni =
  let p = perspective (pi / 3) (let V2 w h = uni.windowSize in toFloat w / toFloat h) 0.01 (-1)
      v = lookAt' uni.viewCamera uni.viewTarget uni.viewUp
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

invertModelViewProjection :: (Floating (ConvertFloat a), Convert a) => GlobalUniform a (ConvertFloat a) -> M44 (ConvertFloat a)
invertModelViewProjection uni = inv44 $ viewProjection uni

textShader ::
  Window os RGBAFloat DepthStencil ->
  ApplicationUniforms os ->
  Shader os (TextInput os) ()
textShader win unis = do
  u <- readGlobalUniform unis
  let ws = toFloat <$> (u.windowSize :: V2 (S V Int))
      wh = ws / 2
      edge = (pure ClampToEdge, 1.0)
  samp :: Sampler2D (Format RGBAFloat) <- newSampler2D $ \(TextInput _ _ tex) -> (tex, SamplerFilter Linear Linear Linear (Just 16), edge)
  prims :: PrimitiveStream p (V2 VFloat, V2 VFloat) <- toPrimitiveStream (\(TextInput _ pa _) -> pa)
  let primitiveStream :: PrimitiveStream p (VPos, (V4 VFloat, V2 VFloat)) = fmap (\(v, uv) -> let v' = (v - wh) / wh in (V4 (v' ^. _x) (-(v' ^. _y)) 0 1, (V4 1 1 0 1, uv))) prims
  fragmentStream :: FragmentStream (V4 FFloat, V2 FFloat) <- rasterize (\(TextInput ri _ _) -> (Front, ViewPort (V2 0 0) ri, DepthRange 0 1)) primitiveStream
  let litFrags :: FragmentStream (FragColor RGBAFloat) = light samp . snd <$> fragmentStream
      colorOption = ContextColorOption (BlendRgbAlpha (FuncAdd, FuncAdd) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors Zero One) (V4 0 0 0 0)) (pure True)
  drawWindowColor (const (win, colorOption)) litFrags

data GridInput = GridInput (V2 Int) (PrimitiveArray Lines (B3 Float))

gridShader ::
  Window os RGBAFloat DepthStencil ->
  ApplicationUniforms os ->
  GameCtx os (CompiledShader os (V2 Int))
gridShader win unis = GameCtx $ do
  let gridColor :: V4 Float = V4 0.75 0.75 0.75 1
  let n = 10
  let v = join [[V3 x 0 (-n), V3 x 0 n, V3 (-n) 0 x, V3 n 0 x] | x <- [-n .. n]]
  vbuf :: Buffer os (B3 Float) <- newBuffer (length v)
  writeBuffer vbuf 0 v
  s <- compileShader $ do
    uni <- readGlobalUniform unis
    prims <- toPrimitiveStream $ \(GridInput _ st) -> (\v' -> (v', gridColor)) <$> st
    let projected = (\(p, c) -> let (v', _) = proj uni (p, V3 0 1 0) in (v', c)) <$> prims
    fragmentStream <- rasterize (\(GridInput vp _) -> (Front, ViewPort (V2 0 0) vp, DepthRange 0 1)) projected
    let colorOption = ContextColorOption (BlendRgbAlpha (FuncAdd, FuncAdd) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors Zero One) (V4 0 0 0 0)) (pure True)
    drawWindowColor (const (win, colorOption)) fragmentStream
  return $ \vpSize -> do
    pArr <- newVertexArray vbuf
    let pa = toPrimitiveArray LineList pArr
    s $ GridInput vpSize pa
