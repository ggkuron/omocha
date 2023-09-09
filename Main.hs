{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant irrefutable pattern" #-}

module Main where

import Codec.Picture qualified as P
import Codec.Picture.Types qualified as P
import Control.Lens hiding (indices)
import Control.Monad (join)
import Control.Monad.Exception (MonadException)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word32)
import FRP.Elerea.Param qualified as E
import Graphics.GPipe
import Graphics.GPipe.Context.GLFW qualified as GLFW
import Graphics.GPipe.Context.GLFW.Input qualified as Input
import Omocha.Bitmap
import Omocha.Collada
import Omocha.Font
import Omocha.Scene
import Paths_omocha

loadBitmapsWith [|getDataFileName|] "./static/images"

loadImage ::
  (MonadIO m, ContextHandler ctx) =>
  Bitmap ->
  ContextT ctx os m (Texture2D os (Format RGBAFloat))
loadImage bmp = do
  tex <- newTexture2D RGBA8 size maxBound
  writeTexture2D tex 0 0 size $ P.pixelFold getJuicyPixel [] img
  return tex
  where
    (size, img) = getImage bmp
    getJuicyPixel xs _ _ pix = let P.PixelRGBA8 r g b a = P.convertPixel pix in V4 r g b a : xs

getImage :: Bitmap -> (V2 Int, P.Image P.PixelRGBA8)
getImage bmp =
  let img = bitmapImage bmp
      size = V2 (P.imageWidth img) (P.imageHeight img)
   in (size, img)

scene :: Scene
scene =
  Scene
    { camera = V3 0 0.25 1,
      meshes =
        [ Mesh
            [ DrawVertex (V3 1 0 1) (V3 0 1 0) (V2 1 1),
              DrawVertex (V3 1 0 (-1)) (V3 0 1 0) (V2 1 0),
              DrawVertex (V3 (-1) 0 1) (V3 0 1 0) (V2 0 1),
              DrawVertex (V3 (-1) 0 (-1)) (V3 0 1 0) (V2 0 0)
            ]
            Nothing
            (V3 0 0 0)
            (Just _maptips_grass_png)
            BoardShader,
          Mesh
            [ DrawVertex (V3 1 1 0) (V3 0 1 0) (V2 1 1),
              DrawVertex (V3 1 (-1) 0) (V3 0 1 0) (V2 1 0),
              DrawVertex (V3 (-1) 1 0) (V3 0 1 0) (V2 0 1),
              DrawVertex (V3 (-1) (-1) 0) (V3 0 1 0) (V2 0 0)
            ]
            Nothing
            (V3 0 1 0)
            (Just _front0_png)
            TargetBoard
        ]
    }

buildRendering ::
  forall ctx os m.
  (ContextHandler ctx, MonadIO m, MonadException m) =>
  Window os RGBAFloat Depth ->
  Buffer os (Uniform UniInput) ->
  Scene ->
  ContextT ctx os m (CompiledShader os (V2 Int))
buildRendering win uniform Scene {meshes} = do
  bs <- compileShader $ boardShader (const id) win uniform
  ts <- compileShader $ boardShader (\uni (p, n, uv) -> (p + viewTarget uni, n, uv)) win uniform
  ms <- compileShader $ monoShader (const id) win uniform
  rr <- mapM (renderMesh bs ts ms) meshes
  return $ \vpSize -> mapM_ (\r -> r vpSize) rr
  where
    renderMesh ::
      CompiledShader os (RenderInput os TextureInput) ->
      CompiledShader os (RenderInput os TextureInput) ->
      CompiledShader os (RenderInput os PlainInput) ->
      Mesh ->
      ContextT ctx os m (CompiledShader os (V2 Int))
    renderMesh bs ts ms Mesh {..} = do
      vbuf :: Buffer os VBuffer <- newBuffer (length vertices)
      writeBuffer vbuf 0 [(dvPosition + offset, dvNormal, dvUv) | DrawVertex {..} <- vertices]
      ibuf <- case indices of
        Just indices' -> do
          ibuf :: Buffer os (B Word32) <- newBuffer (length indices')
          writeBuffer ibuf 0 $ map fromIntegral indices'
          return $ Just ibuf
        _ -> return Nothing

      case textureImage of
        Just tex -> do
          tex' <- loadImage tex
          let shader = case shaderType of
                BoardShader -> bs
                TargetBoard -> ts
          return $ \vpSize -> do
            prims <- newPrimitiveArray vbuf ibuf
            shader $ RenderInput vpSize prims tex'
        _ -> do
          let shader = ms
          return $ \vpSize -> do
            prims <- newPrimitiveArray vbuf ibuf
            shader $ PlainInput vpSize prims

    newPrimitiveArray :: forall b i a. (BufferFormat b, Integral i, IndexFormat b ~ i) => Buffer os a -> Maybe (Buffer os b) -> Render os (PrimitiveArray Triangles a)
    newPrimitiveArray p Nothing = do
      pArr <- newVertexArray p
      return $ toPrimitiveArray TriangleStrip pArr
    newPrimitiveArray p (Just i) = do
      pArr <- newVertexArray p
      iArr <- newIndexArray i Nothing
      return $ toPrimitiveArrayIndexed TriangleStrip iArr pArr

main :: IO ()
main = runContextT (GLFW.defaultHandleConfig {GLFW.configEventPolicy = Nothing}) $ do
  win <- newWindow (WindowFormatColorDepth RGBA8 Depth16) (GLFW.defaultWindowConfig "omocha")
  uniform :: Buffer os (Uniform UniInput) <- newBuffer 1

  -- font <- loadFont "VL-PGothic-Regular.ttf"
  collada <- liftIO $ readColladaFile "untitled.dae"
  let s = sceneFromCollada collada

  r <- buildRendering win uniform s
  r0 <- buildRendering win uniform scene

  let renderings =
        [ \vpSize -> do
            clearWindowColor win (V4 0 0.25 1 0)
            clearWindowDepth win 1
            r vpSize
            r0 vpSize
        ]

  (keyInput, keyInputSink) <- liftIO $ E.external (False, False, False, False)

  network <- liftIO $ E.start $ do
    frameCount <- E.stateful 0 (const (+ 1))
    fps <- do
      sig <-
        E.transfer
          (30, (0, 0))
          ( \dt v (x, (v0, t)) ->
              let t' = dt + t
               in if t' > 1
                    then ((v - v0) / t', (v, 0))
                    else (x, (v0, t'))
          )
          frameCount
      return (fst <$> sig)
    moveUnit <- E.stateful 0 (\dt _ -> realToFrac $ 1.25 * dt)

    aa <- keyInput
    target <-
      E.transfer2
        (V3 0 0 0)
        ( \_ (keyH, keyJ, keyK, keyL) mu t ->
            t
              & _x
                +~ ( if
                         | keyL -> -mu
                         | keyH -> mu
                         | otherwise -> 0
                   )
              & _z
                +~ ( if
                         | keyJ -> -mu
                         | keyK -> mu
                         | otherwise -> 0
                   )
        )
        aa
        moveUnit
    camera' <-
      E.transfer
        (V3 0 0.25 1)
        ( \_ t _ ->
            t
              & _y +~ 5
              & _z -~ 7.5
        )
        target
    return $ renderFrame win uniform renderings <$> camera' <*> target <*> fps

  _ <- liftIO $ GLFW.setTime 0
  fix $ \(~loop) -> do
    _ <- GLFW.mainstep win GLFW.Wait
    dt <- readInput win keyInputSink
    case dt of
      Just dt' -> (join . liftIO . network $ dt') >> loop
      Nothing -> return ()

readInput ::
  forall a os c ds m.
  MonadIO m =>
  Window os c ds ->
  ((Bool, Bool, Bool, Bool) -> IO a) ->
  ContextT GLFW.Handle os m (Maybe Double)
readInput win keyInputSink = do
  t' <- liftIO $ do
    t <- GLFW.getTime
    GLFW.setTime 0
    return t

  closeRequested <- GLFW.windowShouldClose win
  closeKeyPressed <- keyIsPressed GLFW.Key'Q

  keyInput <-
    (,,,)
      <$> keyIsPressed GLFW.Key'H
      <*> keyIsPressed GLFW.Key'J
      <*> keyIsPressed GLFW.Key'K
      <*> keyIsPressed GLFW.Key'L
  _ <- liftIO $ keyInputSink keyInput
  return $ if closeRequested == Just True || closeKeyPressed then Nothing else t'
  where
    keyIsPressed :: Input.Key -> ContextT GLFW.Handle os m Bool
    keyIsPressed k = (Just GLFW.KeyState'Pressed ==) <$> GLFW.getKey win k

renderFrame ::
  Window os RGBAFloat Depth ->
  Buffer os (Uniform UniInput) ->
  [CompiledShader os (V2 Int)] ->
  V3 Float ->
  V3 Float ->
  Double ->
  ContextT GLFW.Handle os IO ()
renderFrame win uniform renderings camera target fps = do
  size <- getFrameBufferSize win
  let viewUpNorm = V3 0 1 0
      normMat = identity
      uni = (fromIntegral <$> size, normMat, camera, target, viewUpNorm, fromRational . toRational $ fps)

  writeBuffer uniform 0 [uni]

  mapM_ (\r -> render $ r size) renderings
  swapWindowBuffers win
