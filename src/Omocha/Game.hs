{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Omocha.Game (run, transpose, bundle) where

import Control.Lens ((+~))
import Control.Monad
import Control.Monad.Exception qualified as E (MonadException (throw), throw)
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
import Omocha.Resource
import Omocha.Scene
import Omocha.Shader
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

readMapFile :: FilePath -> IO MapFile
readMapFile path = do
  f <- BS.readFile path
  case decode f of
    Just m -> return m
    _ -> E.throw ("invalid map file: " ++ path :: String)

parseMapFile :: MapFile -> IO (Vector Mesh)
parseMapFile = mapMeshes (-V2 10 10) 1

mapMeshes :: V2 Float -> V2 Float -> MapFile -> IO (Vector Mesh)
mapMeshes offset unit m = do
  d <- either E.throw return $ foldMapM (parseMapDef m.size) m.mapData
  a <- mapM (toMesh offset unit) d
  return $ join a
  where
    tupleToV4 :: (a, a, a, a) -> V4 a
    tupleToV4 (a, b, c, d) = V4 a b c d
    tupleToV2 (a, b) = V2 a b
    toMesh :: V2 Float -> V2 Float -> (BB.Box V2 Int, MapDef) -> IO (Vector Mesh)
    toMesh offset unit (BB.Box a b, n) =
      let size :: V2 Float = liftA2 (*) unit (fmap fromIntegral (b - a))
          start :: V2 Float = liftA2 (*) unit (fmap fromIntegral a) + offset
          yScale = (unit ^. _x + unit ^. _y) / 2
       in case n of
            Block {..} -> return $ cube (V3 (size ^. _x) (height * yScale) (size ^. _y)) (V3 (start ^. _x) (yOffset * yScale) (start ^. _y)) (tupleToV4 color)
            Plane {..} -> return $ plane (V3 (size ^. _x) (n.yOffset * yScale) (size ^. _y)) (V3 (start ^. _x) (yOffset * yScale) (start ^. _y)) (tupleToV4 color)
            RTPrism {..} -> return $ prism top (V3 (size ^. _x) (height * yScale) (size ^. _y)) (V3 (start ^. _x) (yOffset * yScale) (start ^. _y)) (tupleToV4 color)
            Reference r -> do
              m <- case r of
                (Embed m) -> return m
                (External path) -> readMapFile path -- TODO: check recursive recursion
              let unit' :: V2 Float = liftA2 (/) size (fromIntegral <$> tupleToV2 m.size)
               in mapMeshes start unit' m

boardVertex :: [Vertex]
boardVertex =
  [ Vertex (V3 1 1 0) (V3 0 0 1) (V2 1 1),
    Vertex (V3 (-1) 1 0) (V3 0 0 1) (V2 0 1),
    Vertex (V3 1 (-1) 0) (V3 0 0 1) (V2 1 0),
    Vertex (V3 (-1) (-1) 0) (V3 0 0 1) (V2 0 0)
  ]

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

plane :: V3 Float -> V3 Float -> V4 Float -> Vector Mesh
plane (V3 w d h) offset color =
  let hw = w / 2
      hh = h / 2
      hd = d / 2
   in V.map
        (\v -> Mesh v Nothing (offset + V3 hw hd hh) Nothing BoardShader (TopologyTriangles TriangleStrip) (Just color))
        $ V.fromList
          [ [ Vertex (V3 hw hd hh) (V3 0 1 0) (V2 1 1),
              Vertex (V3 hw hd (-hh)) (V3 0 1 0) (V2 1 0),
              Vertex (V3 (-hw) hd hh) (V3 0 1 0) (V2 0 1),
              Vertex (V3 (-hw) hd (-hh)) (V3 0 1 0) (V2 0 0)
            ]
          ]

cube :: V3 Float -> V3 Float -> V4 Float -> Vector Mesh
cube (V3 w d h) offset color =
  let hw = w / 2
      hh = h / 2
      hd = d / 2
   in V.map
        (\v -> Mesh v Nothing (offset + V3 hw hd hh) Nothing BoardShader (TopologyTriangles TriangleStrip) (Just color))
        $ V.fromList
          [ [ Vertex (V3 hw hd hh) (V3 0 1 0) (V2 1 1),
              Vertex (V3 hw hd (-hh)) (V3 0 1 0) (V2 1 0),
              Vertex (V3 (-hw) hd hh) (V3 0 1 0) (V2 0 1),
              Vertex (V3 (-hw) hd (-hh)) (V3 0 1 0) (V2 0 0)
            ],
            [ Vertex (V3 hw hd hh) (V3 1 0 0) (V2 1 1),
              Vertex (V3 hw (-hd) hh) (V3 1 0 0) (V2 1 0),
              Vertex (V3 hw hd (-hh)) (V3 1 0 0) (V2 0 1),
              Vertex (V3 hw (-hd) (-hh)) (V3 1 0 0) (V2 0 0)
            ],
            [ Vertex (V3 (-hw) hd (-hh)) (V3 (-1) 0 0) (V2 1 1),
              Vertex (V3 (-hw) (-hd) (-hh)) (V3 (-1) 0 0) (V2 0 1),
              Vertex (V3 (-hw) hd hh) (V3 (-hh) 0 0) (V2 1 0),
              Vertex (V3 (-hw) (-hd) hh) (V3 (-1) 0 0) (V2 0 0)
            ],
            [ Vertex (V3 hw hd (-hh)) (V3 0 0 (-1)) (V2 1 1),
              Vertex (V3 hw (-hd) (-hh)) (V3 0 0 (-1)) (V2 0 1),
              Vertex (V3 (-hw) hd (-hh)) (V3 0 0 (-1)) (V2 1 0),
              Vertex (V3 (-hw) (-hd) (-hh)) (V3 0 0 (-1)) (V2 0 0)
            ],
            [ Vertex (V3 hw hd hh) (V3 0 0 1) (V2 1 1),
              Vertex (V3 (-hw) hd hh) (V3 0 0 1) (V2 1 0),
              Vertex (V3 hw (-hd) hh) (V3 0 0 1) (V2 0 1),
              Vertex (V3 (-hw) (-hd) hh) (V3 0 0 1) (V2 0 0)
            ],
            [ Vertex (V3 (-hw) (-hd) hh) (V3 0 (-1) 0) (V2 0 1),
              Vertex (V3 (-hw) (-hd) (-hh)) (V3 0 (-1) 0) (V2 0 0),
              Vertex (V3 hw (-hd) hh) (V3 0 (-1) 0) (V2 1 1),
              Vertex (V3 hw (-hd) (-hh)) (V3 0 (-1) 0) (V2 1 0)
            ]
          ]

prism :: TipEdge -> V3 Float -> V3 Float -> V4 Float -> Vector Mesh
prism edge (V3 w d h) offset color =
  let hw = w / 2
      hh = h / 2
      hd = d / 2
   in V.map
        (\v -> Mesh v Nothing (offset + V3 hw hd hh) Nothing BoardShader (TopologyTriangles TriangleStrip) (Just color))
        $ V.fromList
        . fstFilter
        $ [ ( True,
              [ Vertex
                  ( V3
                      hw
                      ( case edge of
                          ColumnMin -> -hd
                          RowMin -> -hd
                          _ -> hd
                      )
                      hh
                  )
                  (V3 0 1 0)
                  (V2 1 1),
                Vertex
                  ( V3
                      hw
                      ( case edge of
                          ColumnMin -> -hd
                          RowMax -> -hd
                          _ -> hd
                      )
                      (-hh)
                  )
                  (V3 0 1 0)
                  (V2 1 0),
                Vertex
                  ( V3
                      (-hw)
                      ( case edge of
                          ColumnMax -> -hd
                          RowMin -> -hd
                          _ -> hd
                      )
                      hh
                  )
                  (V3 0 1 0)
                  (V2 0 1),
                Vertex
                  ( V3
                      (-hw)
                      ( case edge of
                          ColumnMax -> -hd
                          RowMax -> -hd
                          _ -> hd
                      )
                      (-hh)
                  )
                  (V3 0 1 0)
                  (V2 0 0)
              ]
            ),
            ( edge /= ColumnMin,
              fstFilter
                [ ( True,
                    Vertex (V3 hw (-hd) hh) (V3 1 0 0) (V2 1 0)
                  ),
                  ( True,
                    Vertex (V3 hw (-hd) (-hh)) (V3 1 0 0) (V2 0 0)
                  ),
                  ( edge /= RowMin,
                    Vertex (V3 hw hd hh) (V3 1 0 0) (V2 1 1)
                  ),
                  ( edge /= RowMax,
                    Vertex (V3 hw hd (-hh)) (V3 1 0 0) (V2 0 1)
                  )
                ]
            ),
            ( edge /= ColumnMax,
              fstFilter
                [ ( True,
                    Vertex (V3 (-hw) (-hd) (-hh)) (V3 (-1) 0 0) (V2 0 1)
                  ),
                  ( True,
                    Vertex (V3 (-hw) (-hd) hh) (V3 (-1) 0 0) (V2 0 0)
                  ),
                  ( edge /= RowMax,
                    Vertex (V3 (-hw) hd (-hh)) (V3 (-1) 0 0) (V2 1 1)
                  ),
                  ( edge /= RowMin,
                    Vertex (V3 (-hw) hd hh) (V3 (-hh) 0 0) (V2 1 0)
                  )
                ]
            ),
            ( edge /= RowMax,
              fstFilter
                [ ( True,
                    Vertex (V3 hw (-hd) (-hh)) (V3 0 0 (-1)) (V2 0 1)
                  ),
                  ( True,
                    Vertex (V3 (-hw) (-hd) (-hh)) (V3 0 0 (-1)) (V2 0 0)
                  ),
                  ( edge /= ColumnMin,
                    Vertex (V3 hw hd (-hh)) (V3 0 0 (-1)) (V2 1 1)
                  ),
                  ( edge /= ColumnMax,
                    Vertex (V3 (-hw) hd (-hh)) (V3 0 0 (-1)) (V2 1 0)
                  )
                ]
            ),
            ( edge /= RowMin,
              fstFilter
                [ ( True,
                    Vertex (V3 (-hw) (-hd) hh) (V3 0 0 1) (V2 0 0)
                  ),
                  ( True,
                    Vertex (V3 hw (-hd) hh) (V3 0 0 1) (V2 0 1)
                  ),
                  ( edge /= ColumnMax,
                    Vertex (V3 (-hw) hd hh) (V3 0 0 1) (V2 1 0)
                  ),
                  ( edge /= ColumnMin,
                    Vertex (V3 hw hd hh) (V3 0 0 1) (V2 1 1)
                  )
                ]
            ),
            ( True,
              [ Vertex (V3 (-hw) (-hd) hh) (V3 0 (-1) 0) (V2 0 1),
                Vertex (V3 (-hw) (-hd) (-hh)) (V3 0 (-1) 0) (V2 0 0),
                Vertex (V3 hw (-hd) hh) (V3 0 (-1) 0) (V2 1 1),
                Vertex (V3 hw (-hd) (-hh)) (V3 0 (-1) 0) (V2 1 0)
              ]
            )
          ]
  where
    fstFilter :: [(Bool, a)] -> [a]
    fstFilter = map snd . filter fst

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
                        [ Vertex (V3 1 0 1) (V3 0 1 0) (V2 1 1),
                          Vertex (V3 1 0 (-1)) (V3 0 1 0) (V2 1 0),
                          Vertex (V3 (-1) 0 1) (V3 0 1 0) (V2 0 1),
                          Vertex (V3 (-1) 0 (-1)) (V3 0 1 0) (V2 0 0)
                        ]
                        Nothing
                        (V3 0 0 0)
                        (Just _maptips_grass_png)
                        BoardShader
                        (TopologyTriangles TriangleStrip)
                        Nothing,
                      Mesh
                        boardVertex
                        Nothing
                        (V3 0 1 0)
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
  ApplicationUniforms os ->
  SceneObject ->
  ContextT ctx os m (ObjectId, CompiledShader os GlobalUniformB)
buildRenderer win unis obj = do
  bs <- compileShader $ boardShader win unis
  ts <- compileShader $ boardShader win unis
  ms <- compileShader $ monoShader win unis
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

  rs' <- rankBundle rs
  return (obj.id, rs')

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
      forall os.
      (GameCtx os)
        ( GameOrder ->
          GlobalUniformB ->
          [(ObjectId, ObjectUniformB)] ->
          GameCtx os (),
          Maybe (BB.Box V2 Double) -> ShaderInput -> GameCtx os (),
          Env os
        ),
    network ::
      forall os.
      ( GameOrder -> GlobalUniformB -> [(ObjectId, ObjectUniformB)] -> GameCtx os (),
        Maybe (BB.Box V2 Double) -> ShaderInput -> GameCtx os (),
        Env os
      ) ->
      E.SignalGen Double (E.Signal Input) ->
      IO (Double -> IO (GameCtx os ()))
  }

bundle :: (Foldable t, Monad n) => t (b -> n ()) -> (b -> n ())
bundle fs b = mapM_ (\f' -> f' b) fs

transpose :: (HasCallStack) => Vector (Vector a) -> Vector (Vector a)
transpose v = case V.uncons v of
  Nothing -> V.empty
  Just (x, xss) -> case V.uncons x of
    Nothing -> transpose xss
    Just (x', xs) ->
      let (hds, tls) = V.unzip $ V.map (\x'' -> (V.head x'', V.tail x'')) xss
       in (x' `V.cons` hds) `V.cons` transpose (xs `V.cons` tls)

rankBundle :: (Monad m, Monad n) => Vector (Vector (b -> n ())) -> m (b -> n ())
rankBundle fs = return . bundle $ (join . transpose $ fs)

meshFromGltf :: GLTF.Gltf -> Vector Mesh
meshFromGltf j = V.concatMap (\(GLTF.Mesh _meshName prims _wieghts) -> trace (show _wieghts) $ processMeshPrimitive <$> prims) (gltfMeshes j)
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
        unis <- GameCtx newUniforms
        font <- loadFont "VL-PGothic-Regular.ttf"
        json <- liftIO $ GLTF.fromJsonFile "monkey.gltf"
        j <- liftIO $ case json of
          Left e -> E.throw . userError $ show e
          Right v' -> return v'

        textureStorage <- liftIO $ newIORef M.empty
        fpsSetting <- liftIO $ newIORef 30
        lastRenderTime <- liftIO $ newIORef 0

        let player =
              SceneObject
                { id = ObjectId 0,
                  meshes = meshFromGltf j
                }
        mapFileMeshes <- liftIO $ parseMapFile mf
        let meshes =
              SceneObject
                { id = ObjectId 1,
                  meshes =
                    V.concatMap (.meshes) scene.objects V.++ mapFileMeshes
                }
        rother <- GameCtx $ buildRenderer win unis meshes
        rp <- GameCtx $ buildRenderer win unis player
        gs <- gridShader win unis
        renderings <- liftIO $ newIORef $ renderWith unis [rother, rp]
        ts <- GameCtx $ compileShader $ textShader win unis
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
            clear vpSize = do
              clearWindowColor win (V4 0 0.25 1 1)
              clearWindowDepthStencil win 1 0
              gs vpSize

        let update c g o = GameCtx
              $ case c of
                GameReset -> do
                  mf <- liftIO $ do
                    f <- readMapFile "map.json"
                    parseMapFile f

                  let others =
                        SceneObject
                          { id = ObjectId 1,
                            meshes =
                              V.concatMap (.meshes) scene.objects V.++ mf
                          }
                  rother' <- buildRenderer win unis others
                  let r = renderWith unis [rother', rp]
                  liftIO $ writeIORef renderings r
                  render $ clear g.windowSize
                  r g o
                GameSave i -> do
                  liftIO $ BS.writeFile ("map" ++ show i ++ ".json") $ encode mf
                  r <- liftIO $ readIORef renderings
                  render $ clear g.windowSize
                  r g o
                GameContinue -> do
                  r <- liftIO $ readIORef renderings
                  render $ clear g.windowSize
                  r g o
                GameLoad _ -> do
                  r <- liftIO $ readIORef renderings
                  render $ clear g.windowSize
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
              ( \_ (Input {..}) mu t ->
                  if reset
                    then V3 0 0 0
                    else
                      t
                        & _x
                        +~ ( case direction1 of
                               Just DirRight -> mu
                               Just DirLeft -> -mu
                               _ -> 0
                           )
                          & _z
                        +~ ( case direction1 of
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
              ( \_ (Input {..}) mu p ->
                  if reset
                    then 0
                    else case rotation of
                      Just False -> p - mu * 0.025
                      Just True -> p + mu * 0.025
                      _ -> 0
              )
              ki
              moveUnit
          camera' <-
            E.transfer4
              (V3 0 5 7.5)
              ( \_ theta' t (Input {..}) mu p ->
                  if reset
                    then V3 0 5 7.5
                    else
                      let t' =
                            p
                              & _y
                              +~ ( case direction2 of
                                     Just DirRight -> mu
                                     Just DirLeft -> -mu
                                     _ -> 0
                                 )
                                & _z
                              +~ ( case direction2 of
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
        [(ObjectId, ObjectUniformB)] ->
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
          lightDir = V3 (-0.5) 0.5 0.5

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
              | otherwise -> GameContinue
          )
          GlobalUniform {windowSize = sz, modelNorm = normMat, viewCamera = camera, viewTarget = target, viewUp = viewUpNorm, lightDirection = lightDir}
          [(ObjectId 0, ObjectUniform {position = target})]
      _ <- renderText Nothing (sz, inp)
      GameCtx $ swapWindowBuffers win