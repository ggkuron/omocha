{-# HLINT ignore "Use lambda-case" #-}
module Omocha.Map where

import Control.Monad.Exception qualified as E (MonadException (throw), throw)
import Data.Aeson hiding (json)
import Data.BoundingBox qualified as BB
import Data.BoundingBox.V2 qualified as BB
import Data.Map.Strict qualified as M
import Data.Tuple.Extra (both, snd3)
import Data.Vector qualified as V
import Linear.V2
import Linear.V3
import Linear.V4
import Omocha.MapFile
import Omocha.Scene
import Omocha.Shape
import Omocha.Spline
import Omocha.Uniform
import RIO

loadMapFile :: (HasCallStack, Exception String) => FilePath -> IO (MapFile MapDef)
loadMapFile path = do
  f <- eitherDecodeFileStrict path
  case f of
    Right m -> return m
    Left m -> E.throw ("invalid map file: " ++ path ++ "\n" ++ m :: String)

referenceUnit :: Size Int -> Size Float -> V2 Float
referenceUnit msize isize =
  let msize' = fromIntegral <$> uncurry V2 msize
      size' = uncurry V2 isize
   in liftA2 (/) size' msize'

loadReferenceDef :: (Exception String) => MapReference -> IO (MapFile MapDef)
loadReferenceDef (Embed m) = pure m
loadReferenceDef (External path) = loadMapFile path -- TODO: check recursive recursion

loadReference :: (Exception String) => V3 Float -> V3 Float -> Size Float -> MapReference -> IO (Vector (BB.Box V2 Int, MapDef, ObjectId), Vector SceneObject, SplinePairs Float)
loadReference offset unit (ix, iy) r = do
  m <- loadReferenceDef r
  let unit' = unit ^. _xz * referenceUnit m.size (ix, iy)
   in parseMapFile offset unit' m

referenceOffset :: V3 Float -> SplinePairs Float -> Float -> V2 Float -> V3 Float
referenceOffset offset sps yOffset start =
  let offset' = splined sps start + offset ^. _xz
   in V3 (offset' ^. _x) yOffset (offset' ^. _y)

parseShape :: (HasCallStack, Exception String) => V3 Float -> V3 Float -> MapDef -> Size Float -> SplinePairs Float -> (V2 Int, ObjectId) -> IO (Vector Mesh)
parseShape offset unit n isize sps (a, _id) =
  let a' = fromIntegral <$> a
      pos' = ((a' ^. _x, a' ^. _x + fst isize), (a' ^. _y, a' ^. _y + snd isize))
      yScale = unit ^. _y
      divs = uncurry V2 (both ((* 16) . floor) isize)
      colorToV4 t = let (a, b, c, d) = rgba t in V4 a b c d
   in case n of
        Empty -> return V.empty
        Cube {..} -> return $ cube pos' unit height (V3 (offset ^. _x) (offset ^. _y + yOffset * yScale) (offset ^. _z)) (colorToV4 color) sps divs
        Plane {..} -> return $ plane pos' unit n.yOffset (V3 (offset ^. _x) (offset ^. _y + yOffset * yScale) (offset ^. _z)) (colorToV4 color) sps divs
        Slope {..} -> return $ slope pos' unit highEdge (high, low) (V3 (offset ^. _x) (offset ^. _y + yOffset * yScale) (offset ^. _z)) (colorToV4 color) sps divs
        Tetra {..} -> return $ tetra pos' unit edge (high, low) (V3 (offset ^. _x) (offset ^. _y + yOffset * yScale) (offset ^. _z)) (colorToV4 color) sps divs
        Cylinder {..} -> return $ cylinder pos' unit center height (V3 (offset ^. _x) (offset ^. _y + yOffset * yScale) (offset ^. _z)) (colorToV4 color) sps divs
        Cone {..} -> return $ cone pos' unit center height (V3 (offset ^. _x) (offset ^. _y + yOffset * yScale) (offset ^. _z)) (colorToV4 color) sps
        Sphere {..} -> return $ sphere pos' unit height (V3 (offset ^. _x) (offset ^. _y + yOffset * yScale) (offset ^. _z)) (colorToV4 color) sps
        Reference r yOffset -> do
          let offset' = unit * referenceOffset offset sps yOffset (fromIntegral <$> a)
          a <- loadReference offset' unit isize r
          return $ V.concatMap (.meshes) (snd3 a)

toMesh :: (HasCallStack, Exception String) => V3 Float -> V2 Float -> (MapDef, Size Int, Vector (V2 Int, ObjectId)) -> SplinePairs Float -> IO (Vector Mesh)
toMesh offset unit (n, size, pos) sps =
  let unit' = V3 (unit ^. _x) (mapY unit) (unit ^. _y)
   in V.foldMap (parseShape offset unit' n (both fromIntegral size) sps) pos

parseMapFile :: (HasCallStack, Exception String) => V3 Float -> V2 Float -> MapFile MapDef -> IO (Vector (BB.Box V2 Int, MapDef, ObjectId), Vector SceneObject, SplinePairs Float)
parseMapFile offset unit m = do
  ds <- either E.throw return (V.mapM (parseMapDef m.size) m.mapData)
  let sps = spline1 m.size m.splineX m.splineY
  r <-
    V.foldMap
      ( \(a, d) ->
          V.mapM
            ( \(box, _, oid) -> do
                mesh <- toMesh offset unit (d.defs M.! oid) sps
                return
                  $ SceneObject
                    { id = oid,
                      meshes = mesh,
                      bbox = box
                    }
            )
            a
      )
      ds
  return (V.concatMap fst ds, r, sps)

lookupMapDef :: Maps -> BB.Box V2 Float -> Vector (MapDef, BB.Box V2 Float)
lookupMapDef m t = select <$> V.filter (\(bb, _, _) -> any (\c -> BB.isInside c ((fromIntegral <$> bb) `BB.mult` m.unit)) corners) m.defs
  where
    corners = (\t' -> t' - m.offset ^. _xz) <$> BB.corners t
    select :: (BB.Box V2 Int, MapDef, a) -> (MapDef, BB.Box V2 Float)
    select (a, b, _) = (b, (fromIntegral <$> a) `BB.mult` m.unit)

mapY :: (Fractional a, R2 t) => t a -> a
mapY unit = (unit ^. _x + unit ^. _y) / 2

mapHeight :: (Exception String) => Maps -> BB.Box V2 Float -> V3 Float -> IO Float
mapHeight m t@(BB.Box start end) offset = do
  let yScale = mapY m.unit
      defs = lookupMapDef m t
  hs <-
    forM
      defs
      ( \d -> case d of
          (Cube {..}, _) -> return $ yScale * yOffset + height
          (Plane {}, _) -> return 0
          (Slope {..}, BB.Box ts te) ->
            let width = te - ts
             in return
                  ( yOffset + case highEdge of
                      (X, False) -> low + (high - low) * (1 - (center ^. _x - (ts ^. _x)) / (width ^. _x))
                      (X, True) -> low + (high - low) * ((center ^. _x - (ts ^. _x)) / (width ^. _x))
                      (Y, False) -> low + (high - low) * (1 - (center ^. _y - (ts ^. _y)) / (width ^. _y))
                      (Y, True) -> low + (high - low) * ((center ^. _y - (ts ^. _y)) / (width ^. _y))
                  )
          (Tetra {..}, BB.Box ts te) ->
            let width = te - ts
             in return
                  ( yOffset + case edge of
                      (False, False) -> let n = (center ^. _x - (ts ^. _x)) / (width ^. _x) + (center ^. _y - (ts ^. _y)) / (width ^. _y) in if n > 1 then low else low + high * n
                      (False, True) -> let n = -(center ^. _x - (ts ^. _x) / width ^. _x) + (center ^. _y - (ts ^. _y)) / (width ^. _y) in if n < 0 then low else low + high * n
                      (True, False) -> let n = -(center ^. _x - (ts ^. _x) / width ^. _x) + (center ^. _y - (ts ^. _y)) / (width ^. _y) in if n > 0 then low else low + high * (1 - n)
                      (True, True) -> let n = (center ^. _x - (ts ^. _x)) / (width ^. _x) + (center ^. _y - (ts ^. _y)) / (width ^. _y) in if n < 1 then low else low + high * n
                  )
          (Sphere {..}, _) -> return (yScale * height)
          (Cylinder {..}, _) -> return (yScale * height)
          (Cone {..}, _) -> return (yScale * height)
          (Reference r y, BB.Box ts te) -> do
            let size = te - ts
                (V2 sx sy) = size
                offset' = referenceOffset offset m.splines y ts
            mdef <- loadReferenceDef r
            let u'' = referenceUnit mdef.size (sx, sy)
                yScale' = mapY (size / (fromIntegral <$> uncurry V2 mdef.size))
            (m', _, sps) <- loadReference offset' (V3 (u'' ^. _x) (mapY u'') (u'' ^. _y)) (sx, sy) r
            h <- mapHeight (Maps u'' m' offset' sps) t offset'
            return $ yScale' * (h + y)
          (Empty, _) -> return 0
      )
  return $ if null hs then 0 else V.maximum hs
  where
    center :: V2 Float = start + (end - start) / 2 - m.offset ^. _xz
