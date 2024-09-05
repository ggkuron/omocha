module Omocha.Uniform
  ( ApplicationUniforms (..),
    readGlobalUniform,
    GlobalUniform (..),
    ObjectUniform (..),
    newUniforms,
    writeGlobal,
    GlobalUniformS,
    readObjectUniform,
    GlobalUniformB,
    ObjectUniformS,
    ObjectUniformB (..),
    ObjectId (..),
    renderWith,
  )
where

import Control.Monad
import Control.Monad.Exception (MonadException)
import Data.Map.Strict qualified as M
import Graphics.GPipe
import Numeric.Natural
import RIO

data ApplicationUniforms os = ApplicationUniforms
  { global :: Buffer os (Uniform ({- WindowSize -} B2 Int32 {- Normal -}, V3 (B3 Float {- CameraPosition -}), B3 Float {- CameraTarget -}, B3 Float {- CameraUp -}, B3 Float {- LightDirection -}, B3 Float, B3 Float)),
    objects :: Buffer os (Uniform ({- Uniform (V3 (B3 Float) {- Normal -} , -} B3 Float {- Position -}, V4 (B4 Float))),
    sizeOfObjects :: Natural
  }

newtype ObjectId = ObjectId Int
  deriving (Eq, Ord, Show, Num)

data GlobalUniform i f = GlobalUniform
  { windowSize :: V2 i,
    modelNorm :: M33 f,
    viewCamera :: V3 f,
    viewTarget :: V3 f,
    viewUp :: V3 f,
    light :: V3 f,
    lightDirection :: V3 f
  }

type GlobalUniformS a = GlobalUniform (S a Int) (S a Float)

type GlobalUniformB = GlobalUniform Int Float

data ObjectUniform f b = ObjectUniform
  { position :: V3 f,
    proj :: M44 f
  }

type ObjectUniformS a = ObjectUniform (S a Float) (S a Int)

data ObjectUniformB = ObjectUniformB
  { position :: V3 Float,
    proj :: M44 Float,
    visible :: Bool
  }

readGlobalUniform :: ApplicationUniforms os -> Shader os s (GlobalUniformS a)
readGlobalUniform unis = do
  uni <- getUniform (const (unis.global, 0))
  let (windowSize, modelNorm, viewCamera, viewTarget, viewUp, light, lightDirection) = uni
  return $ GlobalUniform windowSize modelNorm viewCamera viewTarget viewUp light lightDirection

readObjectUniform :: ApplicationUniforms os -> ObjectId -> Shader os s (ObjectUniformS a)
readObjectUniform unis (ObjectId id) = do
  uni <- getUniform (const (unis.objects, id))
  let (position, modelProj) = uni
  return $ ObjectUniform position modelProj

writeGlobal :: (MonadIO m, ContextHandler ctx) => ApplicationUniforms os -> GlobalUniformB -> ContextT ctx os m ()
writeGlobal uni g =
  writeBuffer uni.global 0 [(fromIntegral <$> g.windowSize, g.modelNorm, g.viewCamera, g.viewTarget, g.viewUp, g.light, g.lightDirection)]

newUniforms :: (ContextHandler ctx, MonadIO m) => Natural -> ContextT ctx os m (ApplicationUniforms os)
newUniforms numOfObjects = do
  g <- newBuffer 1
  o <- newBuffer (fromIntegral numOfObjects)
  return $ ApplicationUniforms g o numOfObjects

renderWith ::
  (MonadIO m, ContextHandler ctx, Control.Monad.Exception.MonadException m) =>
  ApplicationUniforms os ->
  Vector (CompiledShader os (GlobalUniformB, M.Map ObjectId ObjectUniformB)) ->
  GlobalUniformB ->
  M.Map ObjectId ObjectUniformB ->
  ContextT ctx os m ()
renderWith uni rs g os = do
  writeGlobal uni g
  writeBuffer uni.objects 0 [maybe (V3 0 0 0, identity) (\a -> (a.position, a.proj)) (M.lookup (ObjectId i) os) | i <- [0 .. fromIntegral uni.sizeOfObjects]]
  forM_ rs $ \r -> render (r (g, os))