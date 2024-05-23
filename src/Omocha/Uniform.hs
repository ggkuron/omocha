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
    ObjectUniformB,
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
  { global :: Buffer os (Uniform ({- WindowSize -} B2 Int32 {- Normal -}, V3 (B3 Float {- CameraPosition -}), B3 Float {- CameraTarget -}, B3 Float {- CameraUp -}, B3 Float {- LightDirection -}, B3 Float)),
    objects :: Buffer os (Uniform ({- Uniform (V3 (B3 Float) {- Normal -} , -} B3 Float {- Position -})),
    sizeOfObjects :: Natural
  }

newtype ObjectId = ObjectId Int
  deriving (Eq, Ord, Show)

data GlobalUniform i f = GlobalUniform
  { windowSize :: V2 i,
    modelNorm :: M33 f,
    viewCamera :: V3 f,
    viewTarget :: V3 f,
    viewUp :: V3 f,
    lightDirection :: V3 f
  }

type GlobalUniformS a = GlobalUniform (S a Int) (S a Float)

type GlobalUniformB = GlobalUniform Int Float

newtype ObjectUniform f = ObjectUniform
  { position :: V3 f
  }

type ObjectUniformS a = ObjectUniform (S a Float)

type ObjectUniformB = ObjectUniform Float

readGlobalUniform :: ApplicationUniforms os -> Shader os s (GlobalUniformS a)
readGlobalUniform unis = do
  uni <- getUniform (const (unis.global, 0))
  let (windowSize, modelNorm, viewCamera, viewTarget, viewUp, lightDirection) = uni
  return $ GlobalUniform windowSize modelNorm viewCamera viewTarget viewUp lightDirection

readObjectUniform :: ApplicationUniforms os -> ObjectId -> Shader os s (ObjectUniformS a)
readObjectUniform unis (ObjectId id) = do
  uni <- getUniform (const (unis.objects, id))
  let position = uni
  return $ ObjectUniform position

writeGlobal :: (MonadIO m, ContextHandler ctx) => ApplicationUniforms os -> GlobalUniformB -> ContextT ctx os m ()
writeGlobal uni g =
  writeBuffer uni.global 0 [(fromIntegral <$> g.windowSize, g.modelNorm, g.viewCamera, g.viewTarget, g.viewUp, g.lightDirection)]

newUniforms :: (ContextHandler ctx, MonadIO m) => Natural -> ContextT ctx os m (ApplicationUniforms os)
newUniforms numOfObjects = do
  g <- newBuffer 1
  o <- newBuffer (fromIntegral numOfObjects)
  return $ ApplicationUniforms g o numOfObjects

renderWith ::
  (MonadIO m, ContextHandler ctx, Control.Monad.Exception.MonadException m) =>
  ApplicationUniforms os ->
  CompiledShader os GlobalUniformB ->
  CompiledShader os (V2 Int) ->
  GlobalUniformB ->
  M.Map ObjectId ObjectUniformB ->
  ContextT ctx os m ()
renderWith uni r clear g os = do
  writeGlobal uni g
  writeBuffer uni.objects 0 [maybe (V3 0 0 0) (.position) (M.lookup (ObjectId i) os) | i <- [0 .. fromIntegral uni.sizeOfObjects]]
  render $ clear g.windowSize
  render $ r g
