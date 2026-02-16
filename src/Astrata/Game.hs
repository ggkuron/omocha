{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}

module Astrata.Game (run, transpose, bundle) where

import Astrata.Bitmap
import Astrata.Context
import Astrata.Font
import Astrata.Map
import Astrata.MapFile
  ( BattleField,
    CardInstanceId,
    FieldSide (..),
    initialBattleField,
  )
import Astrata.MapFile qualified as MF
import Astrata.Mesh
import Astrata.Resource
import Astrata.Scene
import Astrata.Shader
import Astrata.Text (text)
import Astrata.Uniform
import Astrata.UserInput
  ( Direction (..),
    Input,
    InputE,
    InputG
      ( cameraLock,
        cancelSelection,
        confirmSelection,
        direction,
        hardReset,
        n,
        reset,
        rotation,
        save,
        stop
      ),
    diffInput,
    edgeInput,
    initialInput,
    initialInputE,
    justInput,
    onlyInput,
    readInput,
  )
import Control.Lens ((+~))
import Control.Monad
import Control.Monad.Exception qualified as E (MonadException (catch))
import Data.BoundingBox qualified as BB
import Data.Map.Strict qualified as M
import Data.Vector qualified as V
import Debug.Trace (trace)
import FRP.Elerea.Param qualified as E
import Graphics.GPipe hiding (trace, transpose)
import Graphics.GPipe.Context.GLFW qualified as GLFW
import Numeric (showFFloat)
import RIO hiding (trace)

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

data Env os = Env
  { win :: Window os RGBAFloat Depth,
    textureStorage :: IORef (M.Map (Double, Char) (Texture2D os (Format RGBAFloat))),
    fpsSetting :: IORef Double,
    lastRenderedTime :: IORef Double,
    maps :: IORef BattleField,
    cards :: IORef (M.Map CardInstanceId ObjectId),
    cardSelection :: IORef (Vector CardInstanceId)
  }

type ShaderInput = (V2 Int, String)

data Position = Position
  { position :: V3 Float,
    direction :: V3 Float,
    inputDirection :: Maybe Astrata.UserInput.Direction
  }

showF :: (RealFloat a) => a -> String
showF a = showFFloat (Just 2) a " "

updateCardSelection :: Vector CardInstanceId -> InputE -> MF.BattleField -> Vector CardInstanceId
updateCardSelection currentSelection input bf =
  let selectCardInput = justInput input.selectCard
      confirmPressed = edgeInput input.confirmSelection
      cancelPressed = edgeInput input.cancelSelection
   in case (selectCardInput, confirmPressed, cancelPressed) of
        (Just cardIndex, False, False) ->
          V.singleton cardIndex
        (Nothing, False, True) ->
          V.empty
        _ -> currentSelection

showV3F :: V3 Float -> String
showV3F = concatMap showF

instance Show Position where
  show Position {..} = "pos: " ++ showV3F position ++ "\n\tdir: " ++ showV3F direction

updatePosition :: Position -> InputE -> IO Position
updatePosition p input =
  if
    | fst input.reset ->
        return
          $ Position
            { position = V3 3 0 0,
              direction = p.direction,
              inputDirection = fst input.direction
            }
    | otherwise -> do
        let d' = case justInput input.direction of
              Just n -> case p.inputDirection of
                Just current' | n == current' -> p.direction
                _ -> case n of
                  DirUp -> p.direction
                  DirDown -> -p.direction
                  DirRight -> fromQuaternion (axisAngle (V3 0 1 0) (-(pi / 2))) !* p.direction
                  DirLeft -> fromQuaternion (axisAngle (V3 0 1 0) (pi / 2)) !* p.direction
              Nothing -> p.direction
            perFrameTime = 1 / 5
            v' =
              min
                (pure 3)
                if
                  | justInput input.stop -> V3 0 0 0
                  | isJust (justInput input.direction) -> d' * if justInput input.speedUp then 3 else 1
                  | otherwise -> 0
            t' = p.position + v' * pure perFrameTime
        return
          $ Position
            { position = t',
              direction = d',
              inputDirection = fst input.direction
            }

buildRenderer ::
  (HasCallStack) =>
  forall ctx os m.
  (ContextHandler ctx, MonadIO m, E.MonadException m) =>
  (Window os RGBAFloat Depth -> ApplicationUniforms os -> ContextT ctx os m (CompiledShader os (V2 Int))) ->
  Window os RGBAFloat Depth ->
  Vector SceneObject ->
  ContextT ctx os m (ApplicationUniforms os, Vector (CompiledShader os (GlobalUniformB, M.Map ObjectId ObjectUniformB)))
buildRenderer init win os = do
  unis <- newUniforms (fromIntegral . length $ os)
  ms <- compileShader $ monoShader win unis
  bs <- compileShader $ boardShader win unis
  init' <- init win unis

  let clear (i, _) = do
        clearWindowColor win (V4 0 0.25 1 1)
        clearWindowDepth win 1
        init' i.windowSize

  r <- forM os $ \obj -> do
    forM obj.meshes $ \(Mesh {..}) -> do
      ibuf <- case indices of
        Just indices' -> do
          let l = length indices'
          ibuf :: Buffer os (B Word32) <- newBuffer l
          when (l > 0) $ writeBuffer ibuf 0 $ map fromIntegral indices'
          pure $ Just ibuf
        _ -> pure Nothing

      let l = length vertices
      vbuf <- newBuffer l
      nbuf <- newBuffer l
      ubuf <- newBuffer l
      when (l > 0) $ do
        writeBuffer vbuf 0 [position + offset | Vertex {..} <- vertices]
        writeBuffer nbuf 0 [normal | Vertex {..} <- vertices]
        writeBuffer ubuf 0 [uv | Vertex {..} <- vertices]

      case texture of
        Just t -> do
          t' <- loadImage t
          let shader' = bs
          pure
            $ V.singleton
              ( \(i, _m) ->
                  case topology of
                    TopologyTriangles p -> do
                      prims <- newPrimitiveArray1 p vbuf ibuf nbuf ubuf
                      shader' $ ShaderEnvironment i.windowSize prims t' obj.id
                    p -> trace ("unsupported topology " ++ show p) $ pure ()
              )
        Nothing -> do
          pure
            $ V.singleton
              ( \(i, _m) ->
                  case topology of
                    TopologyTriangles p -> do
                      prims <- newPrimitiveArray2 p vbuf ibuf nbuf
                      ms $ PlainEnvironment i.windowSize (fromMaybe (V4 0 0 0 0.75) color) obj.id prims
                    p -> trace ("unsupported topology " ++ show p) $ pure ()
              )
  pure
    (unis, clear `V.cons` rankBundle' (V.concatMap id r))

newPrimitiveArray :: forall os b i a t. (BufferFormat b, Integral i, IndexFormat b ~ i) => PrimitiveTopology t -> VertexArray () a -> Maybe (Buffer os b) -> Render os (PrimitiveArray t a)
newPrimitiveArray t pArr Nothing = pure $ toPrimitiveArray t pArr
newPrimitiveArray t pArr (Just i) = do
  iArr <- newIndexArray i Nothing
  return $ toPrimitiveArrayIndexed t iArr pArr

newPrimitiveArray1 :: forall os b i t. (BufferFormat b, Integral i, IndexFormat b ~ i) => PrimitiveTopology t -> Buffer os BPosition -> Maybe (Buffer os b) -> Buffer os BNormal -> Buffer os BUV -> Render os (PrimitiveArray t (BPosition, BNormal, BUV))
newPrimitiveArray1 t p i n u = do
  pArr :: VertexArray () BPosition <- newVertexArray p
  nArr :: VertexArray () BNormal <- newVertexArray n
  uArr :: VertexArray () BUV <- newVertexArray u
  newPrimitiveArray t (zipVertices (\(a, b) c -> (a, b, c)) (zipVertices (,) pArr nArr) uArr) i

newPrimitiveArray2 :: forall os b i t. (BufferFormat b, Integral i, IndexFormat b ~ i) => PrimitiveTopology t -> Buffer os BPosition -> Maybe (Buffer os b) -> Buffer os BNormal -> Render os (PrimitiveArray t (BPosition, BNormal))
newPrimitiveArray2 t p i n = do
  pArr :: VertexArray () BPosition <- newVertexArray p
  nArr :: VertexArray () BNormal <- newVertexArray n
  newPrimitiveArray t (zipVertices (,) pArr nArr) i

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
        win <- GameCtx $ newWindow (WindowFormatColorDepth RGBA8 Depth16) (GLFW.defaultWindowConfig "ASTRATA")
        font <- loadFont "VL-PGothic-Regular.ttf"
        textureStorage <- liftIO $ newIORef M.empty
        fpsSetting <- liftIO $ newIORef 60
        lastRenderTime <- liftIO $ newIORef 0

        let g = initialBattleField
        objs <- liftIO $ parseMapFile g

        maps <- liftIO $ newIORef g
        cards <- liftIO $ newIORef $ M.map (\o -> o.id) objs
        cardSelection <- liftIO $ newIORef V.empty

        let gs = gridShader (7, 8) (V2 1 1.25) (V3 0 0 (-(4 * 1.25)))
        (unis, s) <- GameCtx $ buildRenderer gs win (V.fromList $ M.elems objs)
        ts <- GameCtx $ compileShader $ textShader win unis
        renderings <- liftIO $ newIORef $ renderWith unis s

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
                    ts $ TextEnvironment vpSize pa t
                render $ bundle t ws
        let update c g o = GameCtx
              $ case c of
                GameReset -> do
                  E.catch
                    ( do
                        let g = initialBattleField
                        objs <- liftIO $ parseMapFile g
                        writeIORef maps g

                        (unis, s) <- buildRenderer gs win (V.fromList $ M.elems objs)

                        let r = renderWith unis s
                        liftIO $ writeIORef renderings r
                    )
                    (\e -> trace (show (e :: SomeException)) $ return ())

                  r <- liftIO $ readIORef renderings
                  r g o
                GameSave _ -> do
                  r <- liftIO $ readIORef renderings
                  r g o
                GameContinue -> do
                  r <- liftIO $ readIORef renderings
                  r g o
                GameLoad _ -> do
                  -- TODO: implement loading different maps
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
                maps = maps,
                cards = cards,
                cardSelection = cardSelection
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

          p <-
            E.delay
              ( Position
                  { position = V3 3 0 0,
                    direction = V3 0 0 1,
                    inputDirection = Nothing
                  }
              )
              target
          target <-
            E.effectful3
              (\input _ p -> updatePosition p input)
              ki'
              moveUnit
              p

          _cardSelectionSignal <-
            E.effectful2
              ( \input _ -> do
                  currentBf <- liftIO $ readIORef env.maps
                  currentSelection <- liftIO $ readIORef env.cardSelection
                  let newSelection = updateCardSelection currentSelection input currentBf
                  liftIO $ writeIORef env.cardSelection newSelection
                  return newSelection
              )
              ki'
              target

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
              (V3 3 5 8)
              ( \_ theta (t :: Position) input _ p ->
                  if
                    | input.reset -> V3 3 5 8
                    | input.n == Just 0 -> V3 5 15 0
                    | input.cameraLock -> p
                    | otherwise ->
                        if isJust t.inputDirection
                          then (p + ((t.position - 10 * t.direction) & _y +~ 3)) / 2
                          else
                            let traslated = p - t.position
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
      lightDir = V3 0 0.5 0.5
      light = V3 60 10 60

  fpsSetting' <- liftIO $ readIORef env.fpsSetting
  currentCardSelection <- liftIO $ readIORef env.cardSelection
  let timePerFrame = 1 / fpsSetting'
  current <- liftIO GLFW.getTime
  m <- liftIO $ readIORef env.maps
  cards <- liftIO $ readIORef env.cards
  lastRenderTime' <- liftIO $ readIORef env.lastRenderedTime
  let current' = fromMaybe lastRenderTime' current
  liftIO $ writeIORef env.lastRenderedTime current'
  let delta = current' - lastRenderTime'
      waitTime = timePerFrame - delta
      fps = 1 / delta
  when (waitTime > 0) $ liftIO $ threadDelay $ floor $ waitTime * 1000000
  let inp = "target: " ++ show target ++ "\ncamera: " ++ showV3F camera ++ "\nfps: " ++ showF fps ++ "\nmid:" ++ show input.n ++ "\nselection: " ++ show currentCardSelection
      cc = let c' = easeOutQuart (current' `mod''` 0.75) in 0.5 * realToFrac (sin $ pi * c')
  _ <-
    update
      ( if
          | input.hardReset -> GameReset
          | input.save -> GameSave 1
          | otherwise -> GameContinue
      )
      GlobalUniform {windowSize = sz, modelNorm = normMat, viewCamera = camera, viewTarget = target.position, viewUp = viewUpNorm, light = light, lightDirection = lightDir}
      ( M.fromList
          ( ( cards M.! MF.instanceId m.own.origin,
              ObjectUniformB
                { position = V3 2.5 cc (1.25 * 1.5),
                  proj = rotateCard Astrata.MapFile.Own (MF.isTired m.own.origin)
                }
            )
              : ( cards M.! MF.instanceId m.rival.origin,
                  ObjectUniformB
                    { position = V3 2.5 0 (-(1.25 * 1.5)),
                      proj = rotateCard Astrata.MapFile.Opponent (MF.isTired m.rival.origin)
                    }
                )
              : catMaybes
                [ ( \b ->
                      ( cards M.! MF.instanceId b,
                        ObjectUniformB
                          { position = V3 1 0 1.25,
                            proj = rotateCard Astrata.MapFile.Own (MF.isTired b)
                          }
                      )
                  )
                    <$> MF.maybeSlot m.own.frontL,
                  ( \b ->
                      ( cards M.! MF.instanceId b,
                        ObjectUniformB
                          { position = V3 4 0 1.25,
                            proj = rotateCard Astrata.MapFile.Own (MF.isTired b)
                          }
                      )
                  )
                    <$> MF.maybeSlot m.own.frontR,
                  ( \b ->
                      ( cards M.! MF.instanceId b,
                        ObjectUniformB
                          { position = V3 1 0 (-1.25),
                            proj = rotateCard Astrata.MapFile.Opponent (MF.isTired b)
                          }
                      )
                  )
                    <$> MF.maybeSlot m.rival.frontR,
                  ( \b ->
                      ( cards M.! MF.instanceId b,
                        ObjectUniformB
                          { position = V3 4 0 (-1.25),
                            proj = rotateCard Astrata.MapFile.Opponent (MF.isTired b)
                          }
                      )
                  )
                    <$> MF.maybeSlot m.rival.frontL,
                  ( \b ->
                      ( cards M.! MF.instanceId b,
                        ObjectUniformB
                          { position = V3 1 0 (1.25 * 2.5),
                            proj = rotateCard Astrata.MapFile.Own (MF.isTired b)
                          }
                      )
                  )
                    <$> MF.maybeSlot m.own.backL,
                  ( \b ->
                      ( cards M.! MF.instanceId b,
                        ObjectUniformB
                          { position = V3 4 0 (1.25 * 2.5),
                            proj = rotateCard Astrata.MapFile.Own (MF.isTired b)
                          }
                      )
                  )
                    <$> MF.maybeSlot m.own.backR,
                  ( \b ->
                      ( cards M.! MF.instanceId b,
                        ObjectUniformB
                          { position = V3 1 0 (-(1.25 * 2.5)),
                            proj = rotateCard Astrata.MapFile.Opponent (MF.isTired b)
                          }
                      )
                  )
                    <$> MF.maybeSlot m.rival.backR,
                  ( \b ->
                      ( cards M.! MF.instanceId b,
                        ObjectUniformB
                          { position = V3 4 0 (-(1.25 * 2.5)),
                            proj = rotateCard Astrata.MapFile.Opponent (MF.isTired b)
                          }
                      )
                  )
                    <$> MF.maybeSlot m.rival.backL
                ]
          )
      )
  renderText Nothing (sz, inp)
  GameCtx $ swapWindowBuffers env.win

rc :: (Epsilon a, Floating a) => Bool -> M44 a
rc False = mkTransformation (axisAngle (V3 0 1 0) pi) (V3 1 0 1.3)
rc True = identity

rotateCard :: (Epsilon a, Floating a) => Astrata.MapFile.FieldSide -> Bool -> M44 a
rotateCard Astrata.MapFile.Opponent False = rc True
rotateCard Astrata.MapFile.Opponent True = rc False
rotateCard Astrata.MapFile.Own False = rc False
rotateCard Astrata.MapFile.Own True = rc True

easeOutQuart :: (Real a, Ord a, Floating a) => a -> a
easeOutQuart x = 1 - (1 - x) ** 4