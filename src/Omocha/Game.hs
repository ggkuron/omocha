{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Omocha.Game (run, transpose, bundle) where

import Control.Lens ((+~))
import Control.Monad
import Control.Monad.Exception qualified as E (MonadException (catch, throw), throw)
import Data.Aeson hiding (json)
import Data.BoundingBox qualified as BB
import Data.ByteString.Lazy qualified as BS
import Data.Map.Strict qualified as M
import Data.Vector qualified as V
import Debug.Trace (trace)
import FRP.Elerea.Param qualified as E
import Graphics.GPipe hiding (trace, transpose)
import Graphics.GPipe.Context.GLFW qualified as GLFW
import Omocha.Bitmap
import Omocha.Context
import Omocha.Font
import Omocha.MapFile
import Omocha.Mesh
import Omocha.Resource
import Omocha.Scene
import Omocha.Shader
import Omocha.Shape
import Omocha.Text (text)
import Omocha.Uniform
import Omocha.UserInput
import Paths_omocha
import RIO hiding (trace, traceStack)
import Text.GLTF.Loader (Gltf (gltfMeshes))
import Text.GLTF.Loader qualified as GLTF
import Prelude (userError)

loadBitmapsWith [|getDataFileName|] "../../static/images"

loadImage ::
  (MonadIO m, ContextHandler ctx) =>
  Bitmap ->
  ContextT ctx os m (Texture2D os (Format RGBAFloat))
loadImage bmp = do
  let (siz, img) = getImage bmp
  t <- newTexture2D RGBA8 siz maxBound
  when (siz /= V2 0 0) $ writeTexture2D t 0 0 siz (getPixels img)
  return t

loadMapFile :: FilePath -> IO MapFile
loadMapFile path = do
  f <- BS.readFile path
  case decode f of
    Just m -> return m
    _ -> E.throw ("invalid map file: " ++ path :: String)

parseMapFile :: V2 Float -> MapFile -> IO (Vector Mesh)
parseMapFile offset = mapMeshes offset 1
  where
    mapMeshes :: V2 Float -> V2 Float -> MapFile -> IO (Vector Mesh)
    mapMeshes offset unit m = do
      d <- either E.throw return $ foldMapM (parseMapDef m.size) m.mapData
      a <- mapM (toMesh offset unit) d
      return $ join a
      where
        tupleToV4 :: (a, a, a, a) -> V4 a
        tupleToV4 (a, b, c, d) = V4 a b c d
        (xs, ys) = spline1 m.size m.spline
        toMesh :: V2 Float -> V2 Float -> (BB.Box V2 Int, MapDef) -> IO (Vector Mesh)
        toMesh offset unit (BB.Box a b, n) =
          let isize = b - a
              size :: V2 Float = fmap fromIntegral isize
              yScale = ((unit ^. _x + unit ^. _y) / 2)
              unit' = V3 (unit ^. _x) yScale (unit ^. _y)
              splines = (V.slice (a ^. _x) (isize ^. _x) xs, V.slice (a ^. _y) (isize ^. _y) ys)
              divs = isize
           in case n of
                Cube {..} -> return $ cube unit' (V3 (size ^. _x) height (size ^. _y)) (V3 (offset ^. _x) (yOffset * yScale) (offset ^. _y)) (tupleToV4 color) splines divs
                Plane {..} -> return $ plane unit' (V3 (size ^. _x) n.yOffset (size ^. _y)) (V3 (offset ^. _x) (yOffset * yScale) (offset ^. _y)) (tupleToV4 color) splines divs
                Slope {..} -> return $ slope unit' highEdge (size ^. _x, (high, low), size ^. _y) (V3 (offset ^. _x) (yOffset * yScale) (offset ^. _y)) (tupleToV4 color) splines divs
                Cylinder {..} -> return $ cylinder unit' center (V3 (size ^. _x) height (size ^. _y)) (V3 (offset ^. _x) (yOffset * yScale) (offset ^. _y)) (tupleToV4 color) splines divs
                Cone {..} -> return $ cone unit' center (V3 (size ^. _x) height (size ^. _y)) (V3 (offset ^. _x) (yOffset * yScale) (offset ^. _y)) (tupleToV4 color) splines
                Sphere {..} -> return $ sphere unit' (V3 (size ^. _x) height (size ^. _y)) (V3 (offset ^. _x) (yOffset * yScale) (offset ^. _y)) (tupleToV4 color) splines
                Reference r -> do
                  m <- case r of
                    (Embed m) -> return m
                    (External path) -> loadMapFile path -- TODO: check recursive recursion
                  let unit' = liftA2 (/) size (fromIntegral <$> pairToV2 m.size)
                      offset' = splined2 splines (V2 0 0) + offset
                   in mapMeshes offset' unit' m

data TextMesh = TextMesh
  { vertexes :: [(V2 Float, V2 Float)],
    bitmap :: Bitmap,
    key :: (Double, Char)
  }

charMeshes :: forall os. Font -> Maybe (BB.Box V2 Double) -> Double -> String -> (GameCtx os) [TextMesh]
charMeshes font bbox piel = text font piel renderer bbox
  where
    renderer :: (Char -> BB.Box V2 Double -> Bitmap -> (GameCtx os) TextMesh)
    renderer ch pen bmp =
      return
        $ TextMesh
          (boardVertexes $ realToFrac <$> pen)
          bmp
          (piel, ch)
    boardVertexes :: BB.Box V2 Float -> [(V2 Float, V2 Float)]
    boardVertexes (BB.Box (V2 x y) (V2 x' y')) =
      [ (V2 x y, V2 1 1),
        (V2 x y', V2 1 0),
        (V2 x' y, V2 0 1),
        (V2 x' y', V2 0 0)
      ]

scene :: Scene
scene =
  Scene
    { camera = V3 0 0.25 1,
      objects =
        V.fromList
          [ SceneObject
              { id = ObjectId 0,
                meshes =
                  V.fromList
                    [ Mesh
                        (rect (V3 0 1 0) (V2 1 1) (V3 0.5 0 0.5))
                        Nothing
                        (V3 0 0 0)
                        (Just _maptips_grass_png)
                        BoardShader
                        (TopologyTriangles TriangleStrip)
                        Nothing,
                      Mesh
                        (rect (V3 0 0 1) (V2 2 2) (V3 0 1 0))
                        Nothing
                        (V3 0 0 0)
                        (Just _front0_png)
                        BoardShader
                        (TopologyTriangles TriangleStrip)
                        Nothing
                    ]
              }
          ]
    }

data Env os = Env
  { win :: Window os RGBAFloat DepthStencil,
    textureStorage :: IORef (M.Map (Double, Char) (Texture2D os (Format RGBAFloat))),
    fpsSetting :: IORef Double,
    lastRenderedTime :: IORef Double
  }

type ShaderInput = (V2 Int, String)

buildRenderer ::
  forall ctx os m.
  (ContextHandler ctx, MonadIO m, E.MonadException m) =>
  Window os RGBAFloat DepthStencil ->
  [SceneObject] ->
  ContextT ctx os m (ApplicationUniforms os, CompiledShader os GlobalUniformB)
buildRenderer win os = do
  unis <- newUniforms . fromIntegral . length $ os
  r <- forM os $ \obj -> do
    bs <- compileShader $ boardShader win unis obj.id
    ts <- compileShader $ boardShader win unis obj.id
    ms <- compileShader $ monoShader win unis obj.id
    rs <- forM obj.meshes $ \(Mesh {..}) -> do
      ibuf <- case indices of
        Just indices' -> do
          let l = length indices'
          ibuf :: Buffer os (B Word32) <- newBuffer l
          when (l > 0) $ writeBuffer ibuf 0 $ map fromIntegral indices'
          return $ Just ibuf
        _ -> return Nothing

      let l = length vertices
      case texture of
        Just t -> do
          t' <- loadImage t
          let shader' = case shader of
                BoardShader -> bs
                TargetBoard -> ts
          vbuf <- newBuffer l
          when (l > 0) $ writeBuffer vbuf 0 [(position + offset, normal, uv) | Vertex {..} <- vertices]
          return
            $ V.singleton
            $ \i ->
              case topology of
                TopologyTriangles p -> do
                  prims <- newPrimitiveArray p vbuf ibuf
                  shader' $ RenderInput i.windowSize prims t'
                p -> trace ("unsupported topology " ++ show p) $ return ()
        Nothing -> do
          vbuf <- newBuffer l
          when (l > 0) $ writeBuffer vbuf 0 [(position + offset, normal) | Vertex {..} <- vertices]

          return
            $ V.singleton
            $ \i ->
              case topology of
                TopologyTriangles p -> do
                  prims <- newPrimitiveArray p vbuf ibuf
                  clearWindowStencil win 0
                  let input = PlainInput i.windowSize (fromMaybe (V4 0 0 0 0.75) color) prims
                  ms input
                p -> trace ("unsupported topology " ++ show p) $ return ()
    rankBundle rs
  return
    (unis, bundle r)

newPrimitiveArray :: forall os b i a t. (BufferFormat b, Integral i, IndexFormat b ~ i) => PrimitiveTopology t -> Buffer os a -> Maybe (Buffer os b) -> Render os (PrimitiveArray t a)
newPrimitiveArray t p Nothing = do
  pArr <- newVertexArray p
  return $ toPrimitiveArray t pArr
newPrimitiveArray t p (Just i) = do
  pArr <- newVertexArray p
  iArr <- newIndexArray i Nothing
  return $ toPrimitiveArrayIndexed t iArr pArr

execGame ::
  Game ->
  IO ()
execGame Game {..} =
  runResourceT' . unResource $ runContextT (GLFW.defaultHandleConfig {GLFW.configEventPolicy = Just GLFW.Poll}) $ unCtx $ do
    rs@(_, _, Env {..}) <- prepare

    (keyInput, keyInputSink) <- liftIO $ E.external (Input {direction1 = Nothing, direction2 = Nothing, reset = False, enter = False, rotation = Nothing, n = Nothing, hardReset = False, save = False})
    nw <- liftIO $ network rs keyInput

    _ <- liftIO $ GLFW.setTime 0
    loop win keyInputSink nw
  where
    loop win keyInputSink nw = do
      closeRequested <- GameCtx $ GLFW.windowShouldClose win
      unless (fromMaybe False closeRequested) $ do
        dt <- GameCtx $ readInput win keyInputSink
        case dt of
          Nothing -> return ()
          Just dt' -> (join . liftIO . nw $ dt') >> loop win keyInputSink nw

run :: IO ()
run = execGame game

data GameOrder = GameReset | GameLoad Int | GameContinue | GameSave Int

data Game = Game
  { prepare ::
      (HasCallStack) =>
      forall os.
      (GameCtx os)
        ( GameOrder ->
          GlobalUniformB ->
          M.Map ObjectId ObjectUniformB ->
          GameCtx os (),
          Maybe (BB.Box V2 Double) -> ShaderInput -> GameCtx os (),
          Env os
        ),
    network ::
      (HasCallStack) =>
      forall os.
      ( GameOrder -> GlobalUniformB -> M.Map ObjectId ObjectUniformB -> GameCtx os (),
        Maybe (BB.Box V2 Double) -> ShaderInput -> GameCtx os (),
        Env os
      ) ->
      E.SignalGen Double (E.Signal Input) ->
      IO (Double -> IO (GameCtx os ()))
  }

meshFromGltf :: GLTF.Gltf -> Vector Mesh
meshFromGltf j = V.concatMap (\(GLTF.Mesh _meshName prims _wieghts) -> processMeshPrimitive <$> prims) (gltfMeshes j)
  where
    processMeshPrimitive :: GLTF.MeshPrimitive -> Mesh
    processMeshPrimitive (GLTF.MeshPrimitive indices _material mode normals positions texcoords) =
      Mesh
        { vertices = V.toList $ V.zipWith3 Vertex positions normals texcoords,
          indices = Just . V.toList $ V.map fromIntegral indices,
          offset = V3 1 1 0,
          texture = Nothing,
          topology = case mode of
            GLTF.Points -> TopologyPoints PointList
            GLTF.Lines -> TopologyLines LineList
            GLTF.LineLoop -> TopologyLines LineLoop
            GLTF.LineStrip -> TopologyLines LineStrip
            GLTF.TriangleFan -> TopologyTriangles TriangleFan
            GLTF.TriangleStrip -> TopologyTriangles TriangleStrip
            GLTF.Triangles -> TopologyTriangles TriangleList,
          shader = TargetBoard,
          color =
            do
              i <- _material
              m <- j.gltfMaterials V.!? i
              metallicRoughness <- m.materialPbrMetallicRoughness
              return metallicRoughness.pbrBaseColorFactor
        }

rotationMatrix :: Float -> M44 Float
rotationMatrix theta = m33_to_m44 $ fromQuaternion $ axisAngle (V3 0 1 0) theta

game :: Game
game =
  Game
    { prepare = do
        win <- GameCtx $ newWindow (WindowFormatColorDepthStencilCombined RGBA8 Depth24Stencil8) (GLFW.defaultWindowConfig "omocha")
        font <- loadFont "VL-PGothic-Regular.ttf"
        json <- liftIO $ GLTF.fromJsonFile "monkey.gltf"
        j <- liftIO $ case json of
          Left e -> E.throw . userError $ show e
          Right v' -> return v'

        textureStorage <- liftIO $ newIORef M.empty
        fpsSetting <- liftIO $ newIORef 60
        lastRenderTime <- liftIO $ newIORef 0

        let player =
              SceneObject
                { id = ObjectId 0,
                  meshes = meshFromGltf j
                }
            offset = -V2 10 10
        mapFileMeshes <- liftIO $ parseMapFile offset mf
        let meshes =
              SceneObject
                { id = ObjectId 1,
                  meshes =
                    V.concatMap (.meshes) scene.objects V.++ mapFileMeshes
                }
        (unis, s) <- GameCtx $ buildRenderer win [meshes, player]
        let clear = do
              clearWindowColor win (V4 0 0.25 1 1)
              clearWindowDepthStencil win 1 0
        ts <- GameCtx $ compileShader $ textShader win unis
        gs <- GameCtx $ gridShader win unis mf.size mf.spline offset
        renderings <- liftIO $ newIORef $ renderWith unis s $ \vpSize -> do
          clear
          gs vpSize

        let renderText bbox (ws, inp) = do
              vs <- charMeshes font bbox 24 inp
              GameCtx $ do
                t <- forM vs $ \TextMesh {..} -> do
                  cache <- liftIO $ readIORef textureStorage
                  t <- case M.lookup key cache of
                    Just t -> return t
                    Nothing -> do
                      t <- loadImage bitmap
                      liftIO $ writeIORef textureStorage $ M.insert key t cache
                      return t
                  vbuf <- newBuffer (length vertexes)
                  writeBuffer vbuf 0 vertexes
                  return $ \vpSize -> do
                    pArr <- newVertexArray vbuf
                    let pa = toPrimitiveArray TriangleStrip pArr
                    ts $ TextInput vpSize pa t
                render $ bundle t ws
        let update c g o = GameCtx
              $ case c of
                GameReset -> do
                  E.catch
                    ( do
                        f <- liftIO $ loadMapFile "map.json"
                        mf <- liftIO $ parseMapFile offset f

                        let others =
                              SceneObject
                                { id = ObjectId 1,
                                  meshes =
                                    V.concatMap (.meshes) scene.objects V.++ mf
                                }
                        (unis, s) <- buildRenderer win [others, player]
                        gs <- gridShader win unis f.size f.spline offset
                        let r = renderWith unis s $ \vpSize -> do
                              clear
                              gs vpSize
                        liftIO $ writeIORef renderings r
                    )
                    (\e -> trace (show (e :: SomeException)) $ return ())

                  r <- liftIO $ readIORef renderings
                  r g o
                GameSave i -> do
                  liftIO $ BS.writeFile ("map" ++ show i ++ ".json") $ encode mf
                  r <- liftIO $ readIORef renderings
                  r g o
                GameContinue -> do
                  r <- liftIO $ readIORef renderings
                  r g o
                GameLoad i -> do
                  E.catch
                    ( do
                        f <- liftIO $ loadMapFile $ "map" ++ show i ++ ".json"
                        mf <- liftIO $ parseMapFile offset f

                        let others =
                              SceneObject
                                { id = ObjectId 1,
                                  meshes =
                                    V.concatMap (.meshes) scene.objects V.++ mf
                                }
                        (unis, s) <- buildRenderer win [others, player]
                        gs <- gridShader win unis f.size f.spline offset
                        let r = renderWith unis s $ \vpSize -> do
                              clear
                              gs vpSize
                        liftIO $ writeIORef renderings r
                    )
                    (\e -> trace (show (e :: SomeException)) $ return ())
                  r <- liftIO $ readIORef renderings
                  r g o

        return (update, renderText, Env win textureStorage fpsSetting lastRenderTime),
      network = \(update, renderText, env) keyInput ->
        E.start $ do
          dt <- genDeltaTime
          moveUnit <- E.transfer 0 (\_ dt' _ -> realToFrac $ 3 * dt') dt

          ki <- keyInput
          target <-
            E.transfer2
              (V3 0 0 0)
              ( \_ input mu t ->
                  if
                    | input.reset -> V3 0 0 0
                    | input.n == Just 1 -> V3 0 0 0
                    | otherwise ->
                        t
                          & _x
                          +~ ( case input.direction1 of
                                 Just DirRight -> mu
                                 Just DirLeft -> -mu
                                 _ -> 0
                             )
                            & _z
                          +~ ( case input.direction1 of
                                 Just DirDown -> mu
                                 Just DirUp -> -mu
                                 _ -> 0
                             )
              )
              ki
              moveUnit
          -- p :: (Fractional a) => V3 a -> V3 a -> a -> V3 a
          -- p p0 v0 t = V3 (p0 ^. _x + v0 ^. _x * t) (p0 ^. _y + v0 ^. _y * t - 0.5 * 9.81 * t * t) (p0 ^. _z + v0 ^. _z * t)

          theta <-
            E.transfer2
              0
              ( \_ input mu p ->
                  if input.reset
                    then 0
                    else case input.rotation of
                      Just False -> p - mu * 0.025
                      Just True -> p + mu * 0.025
                      _ -> 0
              )
              ki
              moveUnit
          camera' <-
            E.transfer4
              (V3 0 5 7.5)
              ( \_ theta' t input mu p ->
                  if
                    | input.reset -> V3 0 5 7.5
                    | input.n == Just 0 -> V3 0 30 0
                    | otherwise ->
                        let t' =
                              p
                                & _y
                                +~ ( case input.direction2 of
                                       Just DirRight -> mu
                                       Just DirLeft -> -mu
                                       _ -> 0
                                   )
                                  & _z
                                +~ ( case input.direction2 of
                                       Just DirUp -> mu
                                       Just DirDown -> -mu
                                       _ -> 0
                                   )

                            traslated = t' - t
                            rotated = rotationMatrix theta' !* point traslated
                         in rotated ^. _xyz + t
              )
              theta
              target
              ki
              moveUnit
          return $ updateFrame env update renderText <$> camera' <*> target <*> ki
    }
  where
    genDeltaTime = do
      sig <- E.stateful (0, 0) (\t (_, p) -> (t - p, t))
      return $ fst <$> sig
    updateFrame ::
      forall os.
      Env os ->
      ( GameOrder ->
        GlobalUniformB ->
        M.Map ObjectId ObjectUniformB ->
        GameCtx os ()
      ) ->
      (Maybe (BB.Box V2 Double) -> ShaderInput -> GameCtx os ()) ->
      V3 Float ->
      V3 Float ->
      Input ->
      (GameCtx os) ()
    updateFrame Env {..} update renderText camera target input = do
      sz <- GameCtx $ getFrameBufferSize win
      let viewUpNorm = V3 0 1 0
          normMat = identity
          lightDir = V3 (-0.5) 0.25 0.25

      fpsSetting' <- liftIO $ readIORef fpsSetting
      let timePerFrame = 1 / fpsSetting'
      current <- liftIO GLFW.getTime
      lastRenderTime' <- liftIO $ readIORef lastRenderedTime
      let current' = fromMaybe lastRenderTime' current
      liftIO $ writeIORef lastRenderedTime current'
      let delta = current' - lastRenderTime'
          waitTime = timePerFrame - delta
          fps = 1 / delta
      when (waitTime > 0) $ liftIO $ threadDelay $ floor $ waitTime * 1000000
      let inp = "target: " ++ show target ++ "\ncamera: " ++ show camera ++ "\nfps: " ++ show fps ++ "\nmid:" ++ show input.n
      _ <-
        update
          ( if
              | input.hardReset -> GameReset
              | input.save -> GameSave 1
              | input.n == Just 2 -> GameLoad 2
              | input.n == Just 3 -> GameLoad 3
              | otherwise -> GameContinue
          )
          GlobalUniform {windowSize = sz, modelNorm = normMat, viewCamera = camera, viewTarget = target, viewUp = viewUpNorm, lightDirection = lightDir}
          (M.singleton (ObjectId 0) (ObjectUniform {position = target}))
      _ <- renderText Nothing (sz, inp)
      GameCtx $ swapWindowBuffers win