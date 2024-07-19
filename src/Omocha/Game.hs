{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}

module Omocha.Game (run, transpose, bundle) where

import Control.Lens ((+~))
import Control.Monad
import Control.Monad.Exception qualified as E (MonadException (catch))
import Data.Aeson hiding (json)
import Data.Bits (xor)
import Data.BoundingBox qualified as BB
import Data.BoundingBox.V2 qualified as BB
import Data.ByteString.Lazy qualified as BS
import Data.Map.Strict qualified as M
import Data.Tuple.Extra (both)
import Data.Vector qualified as V
import Debug.Trace (trace)
import FRP.Elerea.Param qualified as E
import Graphics.GPipe hiding (trace, transpose)
import Graphics.GPipe.Context.GLFW qualified as GLFW
import Numeric (showFFloat)
import Omocha.Bitmap
import Omocha.Context
import Omocha.Font
import Omocha.Gltf
import Omocha.Map
import Omocha.MapFile
import Omocha.Mesh
import Omocha.Resource
import Omocha.Scene
import Omocha.Shader
import Omocha.Text (text)
import Omocha.Uniform
import Omocha.UserInput
import RIO hiding (trace, traceStack)
import RIO.FilePath (takeDirectory)

loadImage ::
  (MonadIO m, ContextHandler ctx) =>
  Bitmap ->
  ContextT ctx os m (Texture2D os (Format RGBAFloat))
loadImage bmp = do
  let (siz, img) = getImage bmp
  t <- newTexture2D RGBA8 siz maxBound
  when (siz /= V2 0 0) $ writeTexture2D t 0 0 siz (getPixels img)
  return t

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

position :: Maps -> Position -> V2 Int
position m p =
  let V2 x y = (p.position ^. _xz + BB.center p.bbox - m.offset ^. _xz)
   in V2 (floor $ clamp (x / (m.unit ^. _x)) 0 (fst size')) (floor $ clamp (y / (m.unit ^. _y)) 0 (snd size'))
  where
    size' :: (Float, Float) = both (fromIntegral . (\a -> a - 1) . length) m.splines

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
    bbox :: BB.Box V2 Float,
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

playerBox :: BB.Box V2 Float
playerBox = BB.Box (V2 0.8 (-0.55)) (V2 1 0.15)

updatePosition :: Position -> InputE -> Maps -> IO Position
updatePosition p input m =
  if
    | fst input.reset ->
        return
          $ Position
            { position = V3 0 0 0,
              direction = p.direction,
              bbox = playerBox,
              inputDirection = fst input.direction1,
              state = Grounded,
              a = V3 0 0 0,
              v = V3 0 0 0
            }
    | otherwise -> do
        let (d', keepMoving) = case justInput input.direction1 of
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
                | hovering && isJust (justInput input.direction1) && not keepMoving -> d' * if justInput input.speedUp then 1.5 else 0.5
                | hovering && justInput input.stop -> -p.v
                | justInput input.stop -> V3 0 0 0
                | not hovering && edgeInput input.jump && p.state == Grounded -> (p.a + V3 0 60 0) / 2
                | not hovering && p.state == Jumping -> V3 0 (-9.81) 0
                | otherwise -> V3 0 0 0
            hovering' = hovering `xor` edgeInput input.hover
            v' =
              min
                (pure 3)
                if
                  | grounded && justInput input.stop -> V3 0 0 0
                  | grounded && a' ^. _y == 0 && isJust (justInput input.direction1) -> d' * if justInput input.speedUp then 3 else 1
                  | grounded && a' ^. _y == 0 -> V3 0 (a' ^. _y * perFrameTime) 0 -- 加速度による滑りを消しているが、加速度計算で考慮する方がおそらく良い
                  | otherwise -> p.v + a' * pure perFrameTime
        (t', grounded') <- do
          let t = p.position + v' * pure perFrameTime
          h <- mapHeight m (p.bbox `BB.move` (t ^. _xz)) 0

          let mountable = h < (p.position ^. _y) + 0.5 && h > (p.position ^. _y) - 0.5
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
          return (t', if not mountable && not hovering then p.state == Grounded else t' ^. _y <= h)
        return
          $ Position
            { position = t',
              direction = d',
              bbox = BB.Box (V2 0.8 (-0.55)) (V2 1 0.15),
              inputDirection = fst input.direction1,
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
  Vector SceneObject ->
  ContextT ctx os m (ApplicationUniforms os, CompiledShader os (GlobalUniformB, M.Map ObjectId ObjectUniformB))
buildRenderer win os = do
  unis <- newUniforms . fromIntegral . length $ os
  ms <- compileShader $ monoShader win unis
  bs <- compileShader $ boardShader win unis
  ts <- compileShader $ boardShader win unis

  r <- forM os $ \obj -> do
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
            $ \(i, m) ->
              when
                (maybe False (\o -> o.visible) $ M.lookup obj.id m)
                $ case topology of
                  TopologyTriangles p -> do
                    prims <- newPrimitiveArray p vbuf ibuf
                    shader' $ RenderInput i.windowSize prims t'
                  p -> trace ("unsupported topology " ++ show p) $ return ()
        Nothing -> do
          vbuf <- newBuffer l
          when (l > 0) $ writeBuffer vbuf 0 [(position + offset, normal) | Vertex {..} <- vertices]

          return
            $ V.singleton
            $ \(i, m) ->
              when
                (maybe False (\o -> o.visible) $ M.lookup obj.id m)
                $ case topology of
                  TopologyTriangles p -> do
                    prims <- newPrimitiveArray p vbuf ibuf
                    clearWindowStencil win 0
                    let input = PlainInput i.windowSize (fromMaybe (V4 0 0 0 0.75) color) obj prims
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

game :: (HasCallStack) => Game
game =
  Game
    { prepare = do
        win <- GameCtx $ newWindow (WindowFormatColorDepthStencilCombined RGBA8 Depth24Stencil8) (GLFW.defaultWindowConfig "omocha")
        font <- loadFont "VL-PGothic-Regular.ttf"
        pmesh <- liftIO $ fromGlb "A.glb" --  "blockhuman.glb"
        textureStorage <- liftIO $ newIORef M.empty
        fpsSetting <- liftIO $ newIORef 60
        lastRenderTime <- liftIO $ newIORef 0

        let player =
              SceneObject
                { id = ObjectId 0,
                  meshes = pmesh,
                  bbox = BB.Box (V2 0 0) (V2 1 1)
                }
            offset = V3 0 0 0
            unit = V2 20 20

        let mapFile = "static/maps/group1/group.json"
        g <- liftIO $ loadMapFile mapFile
        (m, objs, sps) <- liftIO $ parseMapFile (takeDirectory mapFile) offset unit g

        maps <- liftIO $ newIORef (Maps unit m offset sps (takeDirectory mapFile))
        (unis, s) <- GameCtx $ buildRenderer win (player `V.cons` objs)
        let clear = do
              clearWindowColor win (V4 0 0.25 1 1)
              clearWindowDepthStencil win 1 0
        ts <- GameCtx $ compileShader $ textShader win unis
        gs <- GameCtx $ gridShader win unis g unit offset
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
                        let mapFile = "static/maps/group1/group.json"
                        (f, (m, objs, sps)) <- liftIO $ loadMap mapFile unit

                        writeIORef maps (Maps unit m offset sps (takeDirectory mapFile))
                        (unis, s) <- buildRenderer win (player `V.cons` objs)

                        gs <- gridShader win unis f unit offset
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
                        let dir = "static/maps/group" ++ show i
                        (f, (m, objs, sps)) <- liftIO $ loadMap (dir ++ "/group.json") unit

                        writeIORef maps (Maps unit m offset sps $ "static/maps/group" ++ show i)
                        (unis, s) <- buildRenderer win (player `V.cons` objs)

                        gs <- gridShader win unis f unit offset
                        let r = renderWith unis s $ \vpSize -> do
                              clear
                              gs vpSize
                        liftIO $ writeIORef renderings r
                    )
                    (\e -> trace (show (e :: SomeException)) $ return ())
                  r <- liftIO $ readIORef renderings
                  r g o

        return
          ( update,
            renderText,
            Env
              { win = win,
                textureStorage = textureStorage,
                fpsSetting = fpsSetting,
                lastRenderedTime = lastRenderTime,
                maps = maps
              }
          ),
      network = \(update, renderText, env) keyInput ->
        E.start $ mdo
          dt <- genDeltaTime
          moveUnit <- E.transfer 0 (\_ dt' _ -> realToFrac $ 3 * dt') dt

          ki <- keyInput
          ki' <-
            E.transfer
              initialInputE
              (\_ i p -> diffInput i $ onlyInput p)
              ki

          maps <- E.effectful $ readIORef env.maps

          p <-
            E.delay
              ( Position
                  { position = V3 0 0 0,
                    direction = V3 0 0 1,
                    bbox = BB.Box (V2 0.8 (-0.55)) (V2 1 0.15),
                    inputDirection = Nothing,
                    a = V3 0 0 0,
                    v = V3 0 0 0,
                    state = Grounded
                  }
              )
              target
          target <-
            E.effectful4
              (\input _ maps p -> updatePosition p input maps)
              ki'
              moveUnit
              maps
              p

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
                    | input.cameraLock -> p
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
                                rotated = fromQuaternion (axisAngle (V3 0 1 0) theta) !* traslated
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
updateFrame env update renderText camera target input = do
  sz <- GameCtx $ getFrameBufferSize env.win
  let viewUpNorm = V3 0 1 0
      normMat = identity
      modelProj =
        (if target.state == Hovering then mkTransformation (axisAngle target.direction (pi / 2)) zero else identity)
          !*! mkTransformation (axisAngle (V3 0 1 0) (unangle (target.direction ^. _zx))) (V3 1 0 (-0.25))
          !*! mkTransformation zero (V3 (-1) 0 0.25)

      lightDir = V3 (-0.5) 0.5 0.5

  fpsSetting' <- liftIO $ readIORef env.fpsSetting
  let timePerFrame = 1 / fpsSetting'
  current <- liftIO GLFW.getTime
  m <- liftIO $ readIORef env.maps
  lastRenderTime' <- liftIO $ readIORef env.lastRenderedTime
  let current' = fromMaybe lastRenderTime' current
  liftIO $ writeIORef env.lastRenderedTime current'
  let delta = current' - lastRenderTime'
      waitTime = timePerFrame - delta
      fps = 1 / delta
      pos = position m target
  when (waitTime > 0) $ liftIO $ threadDelay $ floor $ waitTime * 1000000
  let inp = "target: " ++ show target ++ "\ncamera: " ++ showV3F camera ++ "\nfps: " ++ showF fps ++ "\nmid:" ++ show input.n ++ "\np: " ++ show pos
      visibleBox :: BB.Box V2 Int = BB.Box (V2 0 0) (V2 1 1) `BB.move` pos
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
      ( M.fromAscList
          $ (ObjectId 0, ObjectUniformB {position = splined3 m.splines target.position, proj = modelProj, visible = True})
          : V.toList
            ( V.map
                ( \(_, _, id) ->
                    ( id,
                      ObjectUniformB
                        { position = V3 0 0 0,
                          proj = identity,
                          visible = True
                        }
                    )
                )
                . V.filter (\(box, _, _) -> BB.aabb box visibleBox)
                $ m.defs
            )
      )
  renderText Nothing (sz, inp)
  GameCtx $ swapWindowBuffers env.win
