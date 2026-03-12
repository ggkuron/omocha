{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}

module Astrata.Game (run, transpose, bundle) where

import Astrata.Bitmap
import Astrata.Context
import Astrata.Font
import Astrata.Map
import Astrata.MapFile
  ( CardInstanceId,
    FieldSide (..),
    initialBattleField,
  )
import Astrata.MapFile qualified as MF
import Astrata.Mesh
import Astrata.Resource
import Astrata.Scene
import Astrata.Shader
import Astrata.Shape (plane)
import Astrata.Text (text)
import Astrata.Uniform
import Astrata.UserInput
  ( Direction (..),
    Input,
    InputE,
    InputG
      ( cameraLock,
        direction,
        hardReset,
        n,
        reset,
        rotation,
        save,
        stop
      ),
    diffInput,
    initialInput,
    initialInputE,
    justInput,
    onlyInput,
    readInput,
  )
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
    cards :: IORef (M.Map CardInstanceId ObjectId)
  }

type ShaderInput = (V2 Int, String)

data Position = Position
  { position :: V3 Float,
    direction :: V3 Float,
    inputDirection :: Maybe Direction
  }

showF :: (RealFloat a) => a -> String
showF a = showFFloat (Just 2) a " "

updateCardSelection :: MF.Selection -> InputE -> MF.BattleField -> MF.Selection
updateCardSelection currentSelection input bf =
  case input.direction of
    (Just DirLeft, True) -> MF.prevSelection bf currentSelection
    (Just DirRight, True) -> MF.nextSelection bf currentSelection
    (Just DirUp, True) -> MF.prevInLevel bf currentSelection
    (Just DirDown, True) -> MF.nextInLevel bf currentSelection
    _ -> currentSelection

-- グリッド座標でハイライト範囲を指定する型
data GridRect = GridRect
  { gx :: !Float, -- グリッドX座標（左端）
    gz :: !Float, -- グリッドZ座標（下端）
    gw :: !Float, -- グリッド幅
    gh :: !Float -- グリッド高さ
  }

-- グリッド定数
gridCellW :: Float
gridCellW = 1

gridCellH :: Float
gridCellH = 1.25

gridOffsetZ :: Float
gridOffsetZ = -5

-- GridRect → (ワールド中心座標, スケール)
gridRectToWorld :: GridRect -> (V3 Float, V3 Float)
gridRectToWorld GridRect {..} =
  let cx = (gx + gw / 2) * gridCellW
      cz = (gz + gh / 2) * gridCellH + gridOffsetZ
      sx = gw * gridCellW
      sz = gh * gridCellH
   in (V3 cx 0 cz, V3 sx 1 sz)

-- Opponent側はZ軸をグリッド中心(gz=4)で反転
mirrorGridRect :: GridRect -> GridRect
mirrorGridRect GridRect {..} = GridRect gx (8 - gz - gh) gw gh

-- スロットのグリッド座標（Own側基準）
slotGridRect :: MF.FieldSide -> MF.BattleFieldSlot -> GridRect
slotGridRect side slot = case side of
  MF.Own -> ownSlotRect slot
  MF.Opponent -> mirrorGridRect $ case slot of
    MF.OriginSlot -> ownSlotRect MF.OriginSlot
    MF.FrontL -> ownSlotRect MF.FrontR
    MF.FrontR -> ownSlotRect MF.FrontL
    MF.BackL -> ownSlotRect MF.BackR
    MF.BackR -> ownSlotRect MF.BackL
  where
    ownSlotRect MF.OriginSlot = GridRect 3 6 1 1
    ownSlotRect MF.FrontL = GridRect 1.75 5.25 1 1
    ownSlotRect MF.FrontR = GridRect 4.25 5.25 1 1
    ownSlotRect MF.BackL = GridRect 1.75 6.75 1 1
    ownSlotRect MF.BackR = GridRect 4.25 6.75 1 1

slotWorldPos :: MF.FieldSide -> MF.BattleFieldSlot -> V3 Float
slotWorldPos side slot =
  let (V3 cx _ cz, V3 sx _ sz) = gridRectToWorld $ slotGridRect side slot
   in V3 (cx - sx / 2) 0 (cz - sz / 2)

-- バトルスロット用統合関数
slotUniform :: MF.FieldSide -> MF.BattleFieldSlot -> Bool -> ObjectUniformB
slotUniform side slot tired =
  ObjectUniformB
    { position = slotWorldPos side slot,
      proj = rotateCard side tired
    }

-- サイドのバトルスロットカード一覧を生成
battleSlotEntries :: M.Map CardInstanceId ObjectId -> MF.FieldSide -> MF.Field -> [(ObjectId, ObjectUniformB)]
battleSlotEntries cards side field =
  (cards M.! MF.instanceId field.origin, slotUniform side MF.OriginSlot (MF.isTired field.origin))
    : catMaybes
      [ (\b -> (cards M.! MF.instanceId b, slotUniform side slot (MF.isTired b)))
          <$> MF.maybeSlot sat
        | (slot, sat) <-
            [ (MF.FrontL, field.frontL),
              (MF.FrontR, field.frontR),
              (MF.BackL, field.backL),
              (MF.BackR, field.backR)
            ]
      ]

-- 手札一覧を生成
handEntries :: M.Map CardInstanceId ObjectId -> MF.FieldSide -> V.Vector MF.HandCardInstance -> [(ObjectId, ObjectUniformB)]
handEntries cards side hands =
  [ ( cards M.! MF.instanceId c,
      ObjectUniformB
        { position = handWorldPos side i,
          proj = rotateHandCard side False
        }
    )
    | (i, c) <- zip [0 :: Int ..] (V.toList hands)
  ]

-- スタック一覧を生成
stackEntries :: M.Map CardInstanceId ObjectId -> MF.FieldSide -> V.Vector MF.HandCardInstance -> [(ObjectId, ObjectUniformB)]
stackEntries cards side stk =
  [ ( cards M.! MF.instanceId c,
      ObjectUniformB
        { position = stackWorldPos side i,
          proj = rotateCard side False
        }
    )
    | (i, c) <- zip [0 :: Int ..] (V.toList stk)
  ]

-- 座標計算関数
handWorldPos :: MF.FieldSide -> Int -> V3 Float
handWorldPos side idx = V3 (0.5 + fromIntegral idx * 1.1) 0 z
  where
    z = case side of MF.Own -> 5.0; MF.Opponent -> -5.0

stackWorldPos :: MF.FieldSide -> Int -> V3 Float
stackWorldPos side idx = V3 6.0 (fromIntegral idx * 0.005) z
  where
    z = case side of MF.Own -> 3.75; MF.Opponent -> -3.75

-- サイド全体のグリッド座標
sideGridRect :: MF.FieldSide -> GridRect
sideGridRect MF.Own = GridRect 0 4 7 4
sideGridRect MF.Opponent = mirrorGridRect $ sideGridRect MF.Own

-- ゾーンのグリッド座標
zoneGridRect :: MF.FieldSide -> MF.FieldZone -> GridRect
zoneGridRect side zone = case side of
  MF.Own -> ownZoneRect zone
  MF.Opponent -> mirrorGridRect $ ownZoneRect zone
  where
    ownZoneRect MF.BattleZone = GridRect 1.5 5 4 3
    ownZoneRect MF.HandZone = GridRect 0 7.6 7 0.8
    ownZoneRect MF.DeckZone = GridRect 6 5 1.1 1
    ownZoneRect MF.AuraZone = GridRect 3 6 1 1
    ownZoneRect MF.AstraRecordZone = GridRect 6 6 1 1
    ownZoneRect MF.GuardZone = GridRect 1 4.5 6 1

selectionHighlight :: MF.Selection -> Maybe (V3 Float, V3 Float)
selectionHighlight (MF.SideSelect side) = Just $ gridRectToWorld $ sideGridRect side
selectionHighlight (MF.FiledSelect side zone) = Just $ gridRectToWorld $ zoneGridRect side zone
selectionHighlight (MF.BattleSlotSelect side _ slot) = Just $ gridRectToWorld $ slotGridRect side slot
selectionHighlight (MF.CardSelect _) = Nothing

scaledProj :: V3 Float -> M44 Float -> M44 Float
scaledProj (V3 sx sy sz) m = V4 (V4 sx 0 0 0) (V4 0 sy 0 0) (V4 0 0 sz 0) (V4 0 0 0 1) !*! m

showV3F :: V3 Float -> String
showV3F = concatMap showF

instance Show Position where
  show Position {..} = "pos: " ++ showV3F position ++ "\n\tdir: " ++ showV3F direction

updatePosition :: Position -> Astrata.UserInput.InputE -> IO Position
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
        let d' = case Astrata.UserInput.justInput input.direction of
              Just n -> case p.inputDirection of
                Just current' | n == current' -> p.direction
                _ -> case n of
                  Astrata.UserInput.DirUp -> p.direction
                  Astrata.UserInput.DirDown -> -p.direction
                  Astrata.UserInput.DirRight -> fromQuaternion (axisAngle (V3 0 1 0) (-(pi / 2))) !* p.direction
                  Astrata.UserInput.DirLeft -> fromQuaternion (axisAngle (V3 0 1 0) (pi / 2)) !* p.direction
              Nothing -> p.direction
            perFrameTime = 1 / 5
            v' =
              min
                (pure 3)
                if
                  | Astrata.UserInput.justInput input.stop -> V3 0 0 0
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

    (keyInput, keyInputSink) <- liftIO $ E.external Astrata.UserInput.initialInput

    nw <- liftIO $ network rs keyInput

    _ <- liftIO $ GLFW.setTime 0
    loop win keyInputSink nw
  where
    loop win keyInputSink nw = do
      closeRequested <- GameCtx $ GLFW.windowShouldClose win
      unless (fromMaybe False closeRequested) $ do
        dt <- GameCtx $ Astrata.UserInput.readInput win keyInputSink
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
          Maybe (V3 Float, M44 Float) ->
          GameCtx os (),
          Maybe (BB.Box V2 Double) -> ShaderInput -> GameCtx os (),
          Env os
        ),
    network ::
      forall os.
      ( GameOrder -> GlobalUniformB -> M.Map ObjectId ObjectUniformB -> Maybe (V3 Float, M44 Float) -> GameCtx os (),
        Maybe (BB.Box V2 Double) -> ShaderInput -> GameCtx os (),
        Env os
      ) ->
      E.SignalGen Double (E.Signal Astrata.UserInput.Input) ->
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

        cards <- liftIO $ newIORef $ M.map (\o -> o.id) objs

        let gs = gridShader (7, 8) (V2 1 1.25) (V3 0 0 (-(4 * 1.25)))
            cardsVec = V.fromList (M.elems objs)
            allObjs = cardsVec
        (unis, s) <- GameCtx $ buildRenderer gs win allObjs
        ts <- GameCtx $ compileShader $ textShader win unis
        hs <- GameCtx $ compileShader $ highlightShader win unis
        let hlMesh = V.head $ plane (V3 1 0 1) (V3 (-0.5) 0 (-0.5)) (V4 1 1 1 1)
            hlL = length hlMesh.vertices
            hlOff = hlMesh.offset
        hlVbuf <- GameCtx $ newBuffer hlL
        hlNbuf <- GameCtx $ newBuffer hlL
        GameCtx $ when (hlL > 0) $ do
          writeBuffer hlVbuf 0 [v.position + hlOff | v <- hlMesh.vertices]
          writeBuffer hlNbuf 0 [v.normal | v <- hlMesh.vertices]
        let hlRender g' pos proj' = do
              writeHighlight unis pos proj'
              render $ do
                pArr :: VertexArray () BPosition <- newVertexArray hlVbuf
                nArr :: VertexArray () BNormal <- newVertexArray hlNbuf
                let prims = toPrimitiveArray TriangleStrip (zipVertices (,) pArr nArr)
                hs $ HighlightEnvironment {size = g'.windowSize, color = V4 1 1 1 1, primitives = prims}
        renderings <- liftIO $ newIORef $ \g' os' mHighlight -> do
          renderWith unis s g' os'
          case mHighlight of
            Nothing -> return ()
            Just (V3 hx _ hz, proj') -> hlRender g' (V3 hx 0.02 hz) proj'

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
        let update c g o mHighlight = GameCtx
              $ case c of
                GameReset -> do
                  E.catch
                    ( do
                        let bf = initialBattleField
                        objs <- liftIO $ parseMapFile bf
                        let newCardsVec = V.fromList (M.elems objs)
                        (unis', s') <- buildRenderer gs win newCardsVec
                        newHs <- compileShader $ highlightShader win unis'
                        let newHlRender g' pos proj' = do
                              writeHighlight unis' pos proj'
                              render $ do
                                pArr :: VertexArray () BPosition <- newVertexArray hlVbuf
                                nArr :: VertexArray () BNormal <- newVertexArray hlNbuf
                                let prims = toPrimitiveArray TriangleStrip (zipVertices (,) pArr nArr)
                                newHs $ HighlightEnvironment {size = g'.windowSize, color = V4 1 1 1 1, primitives = prims}
                        let r g' os' mH = do
                              renderWith unis' s' g' os'
                              case mH of
                                Nothing -> return ()
                                Just (V3 hx _ hz, proj') -> newHlRender g' (V3 hx 0.02 hz) proj'
                        liftIO $ writeIORef renderings r
                    )
                    (\e -> trace (show (e :: SomeException)) $ return ())

                  r <- liftIO $ readIORef renderings
                  r g o mHighlight
                GameSave _ -> do
                  r <- liftIO $ readIORef renderings
                  r g o mHighlight
                GameContinue -> do
                  r <- liftIO $ readIORef renderings
                  r g o mHighlight
                GameLoad _ -> do
                  -- TODO: implement loading different maps
                  r <- liftIO $ readIORef renderings
                  r g o mHighlight

        return
          ( update,
            renderText,
            Env
              { win = win,
                textureStorage = textureStorage,
                fpsSetting = fpsSetting,
                lastRenderedTime = lastRenderTime,
                cards = cards
              }
          ),
      network = \(update, renderText, env) keyInput ->
        E.start $ mdo
          dt <- genDeltaTime
          moveUnit <- E.transfer 0 (\_ dt' _ -> realToFrac $ 3 * dt') dt

          ki <- keyInput
          ki' <-
            E.transfer
              Astrata.UserInput.initialInputE
              (\_ i p -> Astrata.UserInput.diffInput i $ Astrata.UserInput.onlyInput p)
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

          mapsSignal <- E.transfer initialBattleField (\_ input current -> if input.hardReset then initialBattleField else current) ki
          cardSelectionSignal <-
            E.transfer2
              (MF.SideSelect Own)
              (\_ input bf sel -> updateCardSelection sel input bf)
              ki'
              mapsSignal

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
                        let traslated = p - t.position
                            rotated = fromQuaternion (axisAngle (V3 0 1 0) theta) !* traslated
                         in rotated ^. _xyz + t.position
              )
              theta
              target
              ki
              moveUnit
          return $ updateFrame env update renderText <$> camera' <*> target <*> ki <*> cardSelectionSignal <*> mapsSignal
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
    Maybe (V3 Float, M44 Float) ->
    GameCtx os ()
  ) ->
  (Maybe (BB.Box V2 Double) -> ShaderInput -> GameCtx os ()) ->
  V3 Float ->
  Position ->
  Astrata.UserInput.Input ->
  MF.Selection ->
  MF.BattleField ->
  (GameCtx os) ()
updateFrame env update renderText camera target input cardSelection m = do
  sz <- GameCtx $ getFrameBufferSize env.win
  let viewUpNorm = V3 0 1 0
      normMat = identity
      lightDir = V3 0 0.5 0.5
      light = V3 60 10 60

  fpsSetting' <- liftIO $ readIORef env.fpsSetting
  let timePerFrame = 1 / fpsSetting'
  current <- liftIO GLFW.getTime
  cards <- liftIO $ readIORef env.cards
  lastRenderTime' <- liftIO $ readIORef env.lastRenderedTime
  let current' = fromMaybe lastRenderTime' current
  liftIO $ writeIORef env.lastRenderedTime current'
  let delta = current' - lastRenderTime'
      waitTime = timePerFrame - delta
      fps = 1 / delta
  when (waitTime > 0) $ liftIO $ threadDelay $ floor $ waitTime * 1000000
  let inp = "target: " ++ show target ++ "\ncamera: " ++ showV3F camera ++ "\nfps: " ++ showF fps ++ "\nmid:" ++ show input.n ++ "\nselection: " ++ show cardSelection
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
          $ battleSlotEntries cards MF.Own m.own
          ++ battleSlotEntries cards MF.Opponent m.rival
          ++ handEntries cards MF.Own m.own.hands
          ++ handEntries cards MF.Opponent m.rival.hands
          ++ stackEntries cards MF.Own m.own.stack
          ++ stackEntries cards MF.Opponent m.rival.stack
      )
      (fmap (\(pos, sc) -> (pos, scaledProj sc identity)) (selectionHighlight cardSelection))
  renderText Nothing (sz, inp)
  GameCtx $ swapWindowBuffers env.win

rc :: (Epsilon a, Floating a) => Bool -> M44 a
rc False = mkTransformation (axisAngle (V3 0 1 0) pi) (V3 1 0 1.3)
rc True = identity

rcHand :: (Epsilon a, RealFloat a) => Bool -> M44 a
rcHand False = mkTransformation (axisAngle (V3 0 1 0) pi * axisAngle (V3 1 0 0) (-(pi / 2))) (V3 1 1 0)
rcHand True = identity

rotateHandCard :: (Epsilon a, RealFloat a) => Astrata.MapFile.FieldSide -> Bool -> M44 a
rotateHandCard Astrata.MapFile.Opponent False = rcHand True
rotateHandCard Astrata.MapFile.Opponent True = rcHand False
rotateHandCard Astrata.MapFile.Own False = rcHand False
rotateHandCard Astrata.MapFile.Own True = rcHand True

rotateCard :: (Epsilon a, Floating a) => Astrata.MapFile.FieldSide -> Bool -> M44 a
rotateCard Astrata.MapFile.Opponent False = rc True
rotateCard Astrata.MapFile.Opponent True = rc False
rotateCard Astrata.MapFile.Own False = rc False
rotateCard Astrata.MapFile.Own True = rc True

easeOutQuart :: (Real a, Ord a, Floating a) => a -> a
easeOutQuart x = 1 - (1 - x) ** 4