{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Omocha.Game (run, transpose, bundle, parseMapFile) where

import Control.Lens ((+~), (-~))
import Control.Monad
import Control.Monad.Exception qualified as E (MonadException (catch, throw), throw)
import Data.Aeson hiding (json)
import Data.Bits (xor)
import Data.BoundingBox qualified as BB
import Data.ByteString.Lazy qualified as BS
import Data.Map.Strict qualified as M
import Data.Tuple.Extra (both, snd3)
import Data.Vector qualified as V
import Debug.Trace (trace)
import FRP.Elerea.Param qualified as E
import Graphics.GPipe hiding (trace, transpose)
import Graphics.GPipe.Context.GLFW qualified as GLFW
import Numeric (showFFloat)
import Omocha.Bitmap
import Omocha.Context
import Omocha.Font
import Omocha.MapFile
import Omocha.Mesh
import Omocha.Resource
import Omocha.Scene
import Omocha.Shader
import Omocha.Shape
import Omocha.Spline
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

loadMapFile :: (HasCallStack) => FilePath -> IO MapFile
loadMapFile path = do
  f <- eitherDecodeFileStrict path
  case f of
    Right m -> return m
    Left m -> E.throw ("invalid map file: " ++ path ++ "\n" ++ m :: String)

tupleToV4 :: (a, a, a, a) -> V4 a
tupleToV4 (a, b, c, d) = V4 a b c d

parseShape :: (HasCallStack) => V3 Float -> V3 Float -> MapDef -> Size Int -> Float -> SplinePairs Float -> V2 Int -> IO (Vector Mesh)
parseShape offset unit n isize yScale sps a =
  let splines = sps
      pos = both (both fromIntegral) ((a ^. _x, a ^. _x + fst isize), (a ^. _y, a ^. _y + snd isize))
      divs = uncurry V2 isize * 16
   in case n of
        Cube {..} -> return $ cube pos unit height (V3 (offset ^. _x) (offset ^. _y + yOffset * yScale) (offset ^. _z)) (tupleToV4 color) splines divs
        Plane {..} -> return $ plane pos unit n.yOffset (V3 (offset ^. _x) (offset ^. _y + yOffset * yScale) (offset ^. _z)) (tupleToV4 color) splines divs
        Slope {..} -> return $ slope pos unit highEdge (high, low) (V3 (offset ^. _x) (offset ^. _y + yOffset * yScale) (offset ^. _z)) (tupleToV4 color) splines divs
        Cylinder {..} -> return $ cylinder pos unit center height (V3 (offset ^. _x) (offset ^. _y + yOffset * yScale) (offset ^. _z)) (tupleToV4 color) splines divs
        Cone {..} -> return $ cone pos unit center height (V3 (offset ^. _x) (offset ^. _y + yOffset * yScale) (offset ^. _z)) (tupleToV4 color) splines
        Sphere {..} -> return $ sphere pos unit height (V3 (offset ^. _x) (offset ^. _y + yOffset * yScale) (offset ^. _z)) (tupleToV4 color) splines
        Reference r yOffset -> do
          m <- case r of
            (Embed m) -> return m
            (External path) -> loadMapFile path -- TODO: check recursive recursion
          let msize = fromIntegral <$> uncurry V2 m.size
              size' :: V2 Float = fromIntegral <$> uncurry V2 isize
              unit' = liftA2 (/) size' msize
              offset' = splined splines (fmap fromIntegral a) + offset ^. _xz
              offset'' = V3 (offset' ^. _x) yOffset (offset' ^. _y)
           in do
                a <- parse offset'' unit' m
                return $ snd3 a

toMesh :: (HasCallStack) => V3 Float -> V2 Float -> MapDefs (Vector (V2 Int)) -> SplinePairs Float -> IO (Vector Mesh)
toMesh offset unit m sps =
  M.foldMapWithKey
    ( \_id (n, size, pos) ->
        let yScale = ((unit ^. _x + unit ^. _y) / 2)
            unit' = V3 (unit ^. _x) yScale (unit ^. _y)
         in V.foldMap (parseShape offset unit' n size yScale sps) pos
    )
    m.defs

parse :: (HasCallStack) => V3 Float -> V2 Float -> MapFile -> IO (Vector (BB.Box V2 Int, MapDef), Vector Mesh, SplinePairs Float)
parse offset unit m = do
  ds <- either E.throw return $ V.mapM (parseMapDef' m.size) m.mapData
  let sps = spline1 m.size m.splineX m.splineY
  r <- V.foldMap (\(_, d) -> toMesh offset unit d sps) ds
  return (V.concatMap fst ds, r, sps)

parseMapFile :: (HasCallStack) => V3 Float -> MapFile -> IO (Vector (BB.Box V2 Int, MapDef), Vector Mesh, SplinePairs Float)
parseMapFile offset = parse offset 1

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

type Maps = (Vector (BB.Box V2 Int, MapDef), V3 Float, SplinePairs Float)

data Env os = Env
  { win :: Window os RGBAFloat DepthStencil,
    textureStorage :: IORef (M.Map (Double, Char) (Texture2D os (Format RGBAFloat))),
    fpsSetting :: IORef Double,
    lastRenderedTime :: IORef Double,
    maps :: IORef Maps
  }

type ShaderInput = (V2 Int, String)

data Position = Position
  { position :: V3 Float,
    direction :: V3 Float,
    inputDirection :: Maybe Omocha.UserInput.Direction,
    a :: V3 Float,
    v :: V3 Float,
    state :: PositionState
  }

data PositionState = Hovering | Jumping | Grounded
  deriving (Show, Eq)

showF :: (RealFloat a) => a -> String
showF a = showFFloat (Just 2) a " "

showV3F :: V3 Float -> String
showV3F = concatMap showF

instance Show Position where
  show Position {..} = "pos: " ++ showV3F position ++ "\n\tdir: " ++ showV3F direction ++ "\n\ta: " ++ showV3F a ++ "\n\tv: " ++ showV3F v ++ "\n\tstate: " ++ show state

updatePosition :: Position -> Input -> Maps -> Position
updatePosition p input (ms, offset, _) =
  if
    | input.reset ->
        Position
          { position = V3 0 0 0,
            direction = p.direction,
            inputDirection = input.direction1,
            state = Grounded,
            a = V3 0 0 0,
            v = V3 0 0 0
          }
    | otherwise ->
        let (d', keepMoving) = case input.direction1 of
              Just n -> case p.inputDirection of
                Just current' | n == current' -> (p.direction, True)
                _ -> case n of
                  DirUp -> (p.direction, False)
                  DirDown -> (-p.direction, False)
                  DirRight -> (fromQuaternion (axisAngle (V3 0 1 0) (-(pi / 2))) !* p.direction, False)
                  DirLeft -> (fromQuaternion (axisAngle (V3 0 1 0) (pi / 2)) !* p.direction, False)
              Nothing -> (p.direction, False)
            perFrameTime = 1 / 5
            hovering = p.state == Hovering
            grounded = p.state == Grounded
            a' =
              if
                | hovering && isJust input.direction1 && not keepMoving -> d' * if input.speedUp then 1.5 else 0.5
                | hovering && input.stop -> -p.v
                | input.stop -> V3 0 0 0
                | not hovering && input.jump && p.state == Grounded -> (p.a + V3 0 60 0) / 2
                | not hovering && p.state == Jumping -> V3 0 (-9.81) 0
                | otherwise -> V3 0 0 0
            hovering' = hovering `xor` input.hover
            v' =
              min
                (pure 3)
                if
                  | grounded && input.stop -> V3 0 0 0
                  | grounded && isJust input.direction1 -> d' * if input.speedUp then 3 else 1
                  | grounded -> V3 0 (a' ^. _y * perFrameTime) 0 -- 加速度による滑りを消しているが、加速度計算で考慮する方がおそらく良い
                  | otherwise -> p.v + a' * pure perFrameTime
            (t', grounded') =
              let t = p.position + v' * pure perFrameTime
                  h = mapHeight ms offset (BB.Box (t ^. _xz & _y -~ 0.55 & _x +~ 0.80) (t ^. _xz & _y +~ 0.15 & _x +~ 1))
                  mountable = h < (p.position ^. _y) + 0.5 && h > (p.position ^. _y) - 0.5
                  t' =
                    ( if
                        | mountable -> t
                        | hovering -> t
                        | otherwise -> t & _xz .~ (p.position ^. _xz)
                    )
                      & _y
                      .~ ( if
                             | hovering ->
                                 if
                                   | mountable -> min h (t ^. _y)
                                   | h > t ^. _y - 0.5 -> (t ^. _y) + 0.5
                                   | otherwise -> t ^. _y
                             | grounded ->
                                 if
                                   | mountable -> max (t ^. _y) h
                                   | otherwise -> p.position ^. _y
                             | otherwise -> max h (t ^. _y)
                         )
               in (t', if not mountable && not hovering then p.state == Grounded else t' ^. _y <= h)
         in Position
              { position = t',
                direction = d',
                inputDirection = input.direction1,
                a = if grounded' then a' & _y .~ 0 else a',
                v = if grounded' then v' & _y .~ 0 else v',
                state =
                  if
                    | hovering' -> Hovering
                    | grounded' -> Grounded
                    | otherwise -> Jumping
              }

buildRenderer ::
  (HasCallStack) =>
  forall ctx os m.
  (ContextHandler ctx, MonadIO m, E.MonadException m) =>
  Window os RGBAFloat DepthStencil ->
  [SceneObject] ->
  ContextT ctx os m (ApplicationUniforms os, CompiledShader os GlobalUniformB)
buildRenderer win os = do
  unis <- newUniforms . fromIntegral . length $ os
  ps <- compileShader $ pointShader win unis
  ls <- compileShader $ lineShader win unis
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
                TopologyLines p -> do
                  pArr <- newVertexArray vbuf
                  let prims = toPrimitiveArray p pArr
                  ls $ LinesInput i.windowSize (fromMaybe (V4 0 0 0 0.75) color) prims
                TopologyPoints p -> do
                  pArr <- newVertexArray vbuf
                  let prims = toPrimitiveArray p pArr
                  ps $ PointsInput i.windowSize (fromMaybe (V4 0 0 0 0.75) color) prims
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
  (HasCallStack) =>
  Game ->
  IO ()
execGame Game {..} =
  runResourceT' . unResource $ runContextT (GLFW.defaultHandleConfig {GLFW.configEventPolicy = Just GLFW.Poll}) $ unCtx $ do
    rs@(_, _, Env {..}) <- prepare

    (keyInput, keyInputSink) <- liftIO $ E.external initialInput

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

game :: (HasCallStack) => Game
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
            offset = -V3 0 0 0
        (m, mapFileMeshes, sps) <- liftIO $ parseMapFile offset mf
        maps <- liftIO $ newIORef (m, offset, sps)

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
        gs <- GameCtx $ gridShader win unis mf.size mf.splineX mf.splineY offset
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
                        (m, mf, sps) <- liftIO $ parseMapFile offset f
                        writeIORef maps (m, offset, sps)

                        let others =
                              SceneObject
                                { id = ObjectId 1,
                                  meshes =
                                    V.concatMap (.meshes) scene.objects V.++ mf
                                }
                        (unis, s) <- buildRenderer win [others, player]
                        gs <- gridShader win unis f.size f.splineX f.splineY offset
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
                        (m, mf, sps) <- liftIO $ parseMapFile offset f
                        writeIORef maps (m, offset, sps)

                        let others =
                              SceneObject
                                { id = ObjectId 1,
                                  meshes =
                                    V.concatMap (.meshes) scene.objects V.++ mf
                                }
                        (unis, s) <- buildRenderer win [others, player]
                        gs <- gridShader win unis f.size f.splineX f.splineY offset
                        let r = renderWith unis s $ \vpSize -> do
                              clear
                              gs vpSize
                        liftIO $ writeIORef renderings r
                    )
                    (\e -> trace (show (e :: SomeException)) $ return ())
                  r <- liftIO $ readIORef renderings
                  r g o

        return (update, renderText, Env win textureStorage fpsSetting lastRenderTime maps),
      network = \(update, renderText, env) keyInput ->
        E.start $ do
          dt <- genDeltaTime
          moveUnit <- E.transfer 0 (\_ dt' _ -> realToFrac $ 3 * dt') dt

          ki <- keyInput
          maps <- E.effectful $ readIORef env.maps

          target <-
            E.transfer3
              ( Position
                  { position = V3 0 0 0,
                    direction = V3 0 0 1,
                    inputDirection = Nothing,
                    a = V3 0 0 0,
                    v = V3 0 0 0,
                    state = Grounded
                  }
              )
              (\_ input _ maps p -> updatePosition p input maps)
              ki
              moveUnit
              maps

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
              ( \_ theta (t :: Position) input mu p ->
                  if
                    | input.reset -> V3 0 2 2.5
                    | input.n == Just 0 -> V3 0 30 0
                    | otherwise ->
                        if isJust t.inputDirection
                          then (p + ((t.position - 10 * t.direction) & _y +~ 3)) / 2
                          else
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

                                traslated = t' - t.position
                                rotated = rotationMatrix theta !* point traslated
                             in rotated ^. _xyz + t.position
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
      Position ->
      Input ->
      (GameCtx os) ()
    updateFrame Env {..} update renderText camera target input = do
      sz <- GameCtx $ getFrameBufferSize win
      let viewUpNorm = V3 0 1 0
          normMat = identity
          modelProj =
            (if target.state == Hovering then mkTransformation (axisAngle target.direction (pi / 2)) zero else identity)
              !*! mkTransformation (axisAngle (V3 0 1 0) (unangle (target.direction ^. _zx))) (V3 1 0 (-0.25))
              !*! mkTransformation zero (V3 (-1) 0 0.25)

          lightDir = V3 (-0.5) 0.5 0.5

      fpsSetting' <- liftIO $ readIORef fpsSetting
      let timePerFrame = 1 / fpsSetting'
      current <- liftIO GLFW.getTime
      (_, _, sps) <- liftIO $ readIORef maps
      lastRenderTime' <- liftIO $ readIORef lastRenderedTime
      let current' = fromMaybe lastRenderTime' current
      liftIO $ writeIORef lastRenderedTime current'
      let delta = current' - lastRenderTime'
          waitTime = timePerFrame - delta
          fps = 1 / delta
      when (waitTime > 0) $ liftIO $ threadDelay $ floor $ waitTime * 1000000
      let inp = "target: " ++ show target ++ "\ncamera: " ++ showV3F camera ++ "\nfps: " ++ showF fps ++ "\nmid:" ++ show input.n
      _ <-
        update
          ( if
              | input.hardReset -> GameReset
              | input.save -> GameSave 1
              | input.n == Just 2 -> GameLoad 2
              | input.n == Just 3 -> GameLoad 3
              | input.n == Just 4 -> GameLoad 4
              | otherwise -> GameContinue
          )
          GlobalUniform {windowSize = sz, modelNorm = normMat, viewCamera = camera, viewTarget = target.position, viewUp = viewUpNorm, lightDirection = lightDir}
          (M.singleton (ObjectId 0) (ObjectUniform {position = splined3 sps target.position, proj = modelProj}))
      _ <- renderText Nothing (sz, inp)
      GameCtx $ swapWindowBuffers win