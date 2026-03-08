{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Astrata.Map where

import Astrata.Bitmap (Bitmap)
import Astrata.Gltf
import Astrata.MapFile
import Astrata.Scene
import Astrata.Shape
import Astrata.Uniform
import Data.Either.Combinators (maybeToLeft)
import Data.Map.Strict qualified as M
import Data.Tuple.Extra (both)
import Data.Vector qualified as V
import Linear.V2
import Linear.V3
import Linear.V4
import RIO

referenceOffset :: Float -> V2 Float -> V3 Float
referenceOffset yOffset start =
  let offset' = start
   in V3 (offset' ^. _x) yOffset (offset' ^. _y)

data ShapeDef
  = Empty
  | Card
      { height :: Float,
        yOffset :: Float,
        bitmap :: Maybe Bitmap
      }
  | Cube
      { height :: Float,
        color :: RGBA,
        yOffset :: Float
      }
  | Plane
      { color :: RGBA,
        yOffset :: Float
      }
  | Sphere
      { height :: Float,
        color :: RGBA,
        yOffset :: Float
      }
  | Gltf FilePath
  | Glb FilePath

parseShape :: (HasCallStack, Exception String) => V3 Float -> ShapeDef -> Size Float -> V2 Int -> IO (Vector Mesh)
parseShape unit n isize a =
  let a' = fromIntegral <$> a
      isize' = uncurry V2 isize
      size :: V2 Float = (unit ^. _xz) * isize'
      start :: V3 Float = unit * V3 (a' ^. _x) 0 (a' ^. _y)
      yScale = unit ^. _y
   in case n of
        Empty -> return V.empty
        Card {..} -> return $ card (V3 (size ^. _x) (height * yScale) (size ^. _y)) (V3 (start ^. _x) (start ^. _y + yOffset * yScale) (start ^. _z)) (maybeToLeft (V4 0 1 1 1) bitmap)
        Cube {..} -> return $ cube (V3 (size ^. _x) (height * yScale) (size ^. _y)) (V3 (start ^. _x) (start ^. _y + yOffset * yScale) (start ^. _z)) (colorToV4 color)
        Plane {..} -> return $ plane (V3 (size ^. _x) (n.yOffset * yScale) (size ^. _y)) (V3 (start ^. _x) (start ^. _y + yOffset * yScale) (start ^. _z)) (colorToV4 color)
        Sphere {..} -> return $ sphere (V3 (size ^. _x) (height * yScale) (size ^. _y)) (V3 (start ^. _x) (start ^. _y + yOffset * yScale) (start ^. _z)) (colorToV4 color)
        Glb path -> do
          let offset' = unit * referenceOffset 0 (fromIntegral <$> a)
          V.map (\m -> (m {offset = offset'} :: Mesh)) <$> fromGlb path
        Gltf path -> do
          let offset' = unit * referenceOffset 0 (fromIntegral <$> a)
          V.map (\m -> (m {offset = offset'} :: Mesh)) <$> fromGltf path
  where
    colorToV4 t = let (a, b, c, d) = rgba t in V4 a b c d

toMesh :: (HasCallStack, Exception String) => V2 Float -> (ShapeDef, Size Int, Vector (V2 Int)) -> IO (Vector Mesh)
toMesh unit (shape, size, pos) =
  let unit' = V3 (unit ^. _x) (mapY unit) (unit ^. _y)
   in V.foldMap (parseShape unit' shape (both fromIntegral size)) pos

satelliteMesh :: (HasCallStack, Exception String) => V2 Float -> SatelliteSlot -> IO (Maybe CardInstanceId, Vector Mesh)
satelliteMesh unit slot = do
  let inst = maybeSlot slot
      cid = instanceId <$> inst
      bitmap = (.bitmap) . cardProp <$> inst
  m <- toMesh unit (Card {height = 0.01, bitmap = bitmap, yOffset = 0}, (1, 1), V.singleton 0)
  return (cid, m)

handCardMesh :: (HasCallStack, Exception String) => V2 Float -> HandCardInstance -> IO (CardInstanceId, Vector Mesh)
handCardMesh unit c = do
  m <- toMesh unit (Card {height = 0.01, bitmap = Just (cardBitmap c), yOffset = 0}, (1, 1), V.singleton 0)
  return (instanceId c, m)

parseMapFile :: (HasCallStack, Exception String) => BattleField -> IO (M.Map CardInstanceId SceneObject)
parseMapFile f = do
  let unit = V2 1 1.25
      ownOriginFace = cardBitmap f.own.origin
      rivalOriginFace = cardBitmap f.rival.origin
  o0 <- toMesh unit (Card {height = 0.1, bitmap = Just ownOriginFace, yOffset = 0}, (1, 1), V.singleton 0)
  r0 <- toMesh unit (Card {height = 0.1, bitmap = Just rivalOriginFace, yOffset = 0}, (1, 1), V.singleton 0)
  cs <- forM [f.own.frontL, f.own.frontR, f.rival.frontL, f.rival.frontR, f.own.backL, f.own.backR, f.rival.backL, f.rival.backR] $ satelliteMesh unit
  hs <- mapM (handCardMesh unit) . V.toList $
          f.own.hands <> f.own.stack <> f.rival.hands <> f.rival.stack
  let battleZoneObjs =
        V.imap (\i (cid, m) -> (cid, SceneObject m (ObjectId i)))
          $  (instanceId f.own.origin, o0)
          `V.cons` ( (instanceId f.rival.origin, r0)
          `V.cons` V.fromList (mapMaybe (\(c, m) -> (\c' -> (c', m)) <$> c) cs))
      handOffset = V.length battleZoneObjs
      handObjs = V.imap (\i (cid, m) -> (cid, SceneObject m (ObjectId (handOffset + i))))
                   (V.fromList hs)
  return . M.fromList . V.toList $ battleZoneObjs <> handObjs

mapY :: (Fractional a, R2 t) => t a -> a
mapY unit = (unit ^. _x + unit ^. _y) / 2