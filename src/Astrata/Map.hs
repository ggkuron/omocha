{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Astrata.Map where

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

parseShape :: (HasCallStack, Exception String) => V3 Float -> ShapeDef -> Size Float -> V2 Int -> IO (Vector Mesh)
parseShape unit n isize a =
  let a' = fromIntegral <$> a
      isize' = uncurry V2 isize
      size :: V2 Float = (unit ^. _xz) * isize'
      start :: V3 Float = unit * V3 (a' ^. _x) 0 (a' ^. _y)
      yScale = unit ^. _y
   in case n of
        Empty -> return V.empty
        Card {..} -> return $ card (V3 (size ^. _x) (height * yScale) (size ^. _y)) (V3 (start ^. _x) (start ^. _y + yOffset * yScale) (start ^. _z)) (maybeToLeft (V4 0 1 1 1) cardFaceId)
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
  let (cid, fid) = cardIds slot
  m <- toMesh unit (Card {height = 0.01, cardFaceId = fid, yOffset = 0}, (1, 1), V.singleton 0)
  return (cid, m)
  where
    cardIds (Assigned b) = (Just b.instanceId, Just b.faceId)
    cardIds _ = (Nothing, Nothing)

parseMapFile :: (HasCallStack, Exception String) => BattleField -> IO (M.Map CardInstanceId SceneObject)
parseMapFile f = do
  let unit = V2 1 1.25
      ownOriginFace = originFaceId f.own.origin.level
      rivalOriginFace = originFaceId f.rival.origin.level
  o0 <- toMesh unit (Card {height = 0.1, cardFaceId = Just ownOriginFace, yOffset = 0}, (1, 1), V.singleton 0)
  r0 <- toMesh unit (Card {height = 0.1, cardFaceId = Just rivalOriginFace, yOffset = 0}, (1, 1), V.singleton 0)
  cs <- forM [f.own.frontL, f.own.frontR, f.rival.frontL, f.rival.frontR, f.own.backL, f.own.backR, f.rival.backL, f.rival.backR] $ satelliteMesh unit
  return
    $ M.fromList
    . V.toList
    $ V.imap (\i (cid, m) -> (cid, SceneObject m (ObjectId i)))
    $ (f.own.origin.instanceId, o0)
    `V.cons` ((f.rival.origin.instanceId, r0) `V.cons` V.fromList (mapMaybe (\(c, m) -> (\c' -> (c', m)) <$> c) cs))

-- ,
-- ( f.own.frontL
-- SceneObject
--   { meshes = fl,
--     id = fieldObjectId Own FrontL
--   },
-- SceneObject
--   { meshes = fr,
--     id = fieldObjectId Own FrontR
--   },
-- SceneObject
--   { meshes = bl,
--     id = fieldObjectId Own BackL
--   },
-- SceneObject
--   { meshes = br,
--     id = fieldObjectId Own BackR
--   }
-- ]

-- lookupMapDef :: Maps -> BB.Box V2 Float -> Vector (MapDef, BB.Box V2 Float)
-- lookupMapDef m t = select <$> V.filter (\(bb, _, _) -> any (\c -> BB.isInside c ((fromIntegral <$> bb) `BB.mult` m.unit)) corners) m.defs
--   where
--     corners = (\t' -> t' - m.offset ^. _xz) <$> BB.corners t
--     select :: (BB.Box V2 Int, MapDef, a) -> (MapDef, BB.Box V2 Float)
--     select (a, b, _) = (b, (fromIntegral <$> a) `BB.mult` m.unit)
--
mapY :: (Fractional a, R2 t) => t a -> a
mapY unit = (unit ^. _x + unit ^. _y) / 2

--
-- mapHeight :: (Exception String) => Maps -> BB.Box V2 Float -> V3 Float -> IO Float
-- mapHeight m t@(BB.Box start end) offset = do
--   let yScale = mapY m.unit
--       defs = lookupMapDef m t
--   hs <-
--     forM
--       defs
--       ( \d -> case d of
--           (Cube {..}, _) -> return (yOffset + height)
--           (Plane {}, _) -> return 0
--           (Sphere {..}, _) -> return (yScale * (yOffset + height))
--           (Reference r y, BB.Box ts te) -> do
--             let size = te - ts
--                 (V2 sx sy) = size
--                 offset' = referenceOffset offset y ts
--             mdef <- loadReferenceDef m.dir r
--             let u'' = referenceUnit mdef.size (sx, sy)
--                 yScale' = mapY u''
--             (m', _) <- loadReference offset' 1 (sx, sy) m.dir r
--             h <- mapHeight (Maps u'' m' offset' m.size m.dir) t offset'
--             return (yScale' * (h + y))
--           (Empty, _) -> return 0
--           (Gltf _, _) -> return 0
--           (Glb _, _) -> return 0
--       )
--   return $ if null hs then 0 else V.maximum hs