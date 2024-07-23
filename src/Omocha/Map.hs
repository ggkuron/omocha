{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Omocha.Map where

import Control.Monad.Exception qualified as E (MonadException (throw), throw)
import Data.Aeson hiding (json)
import Data.BoundingBox qualified as BB
import Data.BoundingBox.V2 qualified as BB
import Data.Map.Strict qualified as M
import Data.Tuple.Extra (both)
import Data.Vector qualified as V
import Linear.V2
import Linear.V3
import Linear.V4
import Omocha.Gltf
import Omocha.MapFile
import Omocha.Scene
import Omocha.Shape
import Omocha.Uniform
import RIO
import RIO.FilePath

loadMapFile :: (HasCallStack, Exception String) => FilePath -> IO MapFile
loadMapFile path = do
  f <- eitherDecodeFileStrict path
  case f of
    Right m -> return m
    Left m -> E.throw ("invalid map file: " ++ path ++ "\n" ++ m :: String)

loadMap :: (HasCallStack, Exception String) => FilePath -> V2 Float -> IO (MapFile, (Vector (BB.Box V2 Int, MapDef, ObjectId), Vector SceneObject))
loadMap path unit = do
  f <- loadMapFile path
  r <- parseMapFile (takeDirectory path) 0 unit f
  return (f, r)

referenceUnit :: Size Int -> Size Float -> V2 Float
referenceUnit msize isize =
  let msize' = fromIntegral <$> uncurry V2 msize
      size' = uncurry V2 isize
   in liftA2 (/) size' msize'

loadReferenceDef :: (Exception String) => FilePath -> MapReference -> IO MapFile
loadReferenceDef _ (Embed m) = pure m
loadReferenceDef dir (External path) = loadMapFile $ dir </> path -- TODO: check recursive recursion

loadReference :: (Exception String) => V3 Float -> V3 Float -> Size Float -> FilePath -> MapReference -> IO (Vector (BB.Box V2 Int, MapDef, ObjectId), Vector SceneObject)
loadReference offset unit (ix, iy) dir r = do
  m <- loadReferenceDef dir r
  let unit' = unit ^. _xz * referenceUnit m.size (ix, iy)
   in parseMapFile dir offset unit' m

referenceOffset :: V3 Float -> Float -> V2 Float -> V3 Float
referenceOffset offset yOffset start =
  let offset' = start + offset ^. _xz
   in V3 (offset' ^. _x) yOffset (offset' ^. _y)

parseShape :: (HasCallStack, Exception String) => FilePath -> V3 Float -> V3 Float -> MapDef -> Size Float -> (V2 Int, ObjectId) -> IO (Vector Mesh)
parseShape workingDir offset unit n isize (a, _id) =
  let a' = fromIntegral <$> a
      isize' = uncurry V2 isize
      size :: V2 Float = (unit ^. _xz) * isize'
      start :: V3 Float = unit * V3 (a' ^. _x) 0 (a' ^. _y) + offset
      yScale = unit ^. _y
   in case n of
        Empty -> return V.empty
        Cube {..} -> return $ cube (V3 (size ^. _x) (height * yScale) (size ^. _y)) (V3 (start ^. _x) (start ^. _y + yOffset * yScale) (start ^. _z)) (colorToV4 color)
        Plane {..} -> return $ plane (V3 (size ^. _x) (n.yOffset * yScale) (size ^. _y)) (V3 (start ^. _x) (start ^. _y + yOffset * yScale) (start ^. _z)) (colorToV4 color)
        Slope {..} -> return $ slope highEdge (size ^. _x, (high * yScale, low * yScale), size ^. _y) (V3 (start ^. _x) (start ^. _y + yOffset * yScale) (start ^. _z)) (colorToV4 color)
        Cylinder {..} -> return $ cylinder center (V3 (size ^. _x) (height * yScale) (size ^. _y)) (V3 (start ^. _x) (start ^. _y + yOffset * yScale) (start ^. _z)) (colorToV4 color)
        Cone {..} -> return $ cone center (V3 (size ^. _x) (height * yScale) (size ^. _y)) (V3 (start ^. _x) (start ^. _y + yOffset * yScale) (start ^. _z)) (colorToV4 color)
        Tetra {..} -> return $ tetra (size ^. _x, (high * yScale, low * yScale), size ^. _y) edge (V3 (start ^. _x) (start ^. _y + yOffset * yScale) (start ^. _z)) (colorToV4 color)
        Sphere {..} -> return $ sphere (V3 (size ^. _x) (height * yScale) (size ^. _y)) (V3 (start ^. _x) (start ^. _y + yOffset * yScale) (start ^. _z)) (colorToV4 color)
        Curve {..} -> return $ curve adjacent (V3 (size ^. _x) (height * yScale) (size ^. _y)) width (V3 (start ^. _x) (start ^. _y + yOffset * yScale) (start ^. _z)) (colorToV4 color)
        Reference r yOffset -> do
          let offset' = unit * referenceOffset offset yOffset a'
          r <- loadReference offset' unit isize workingDir r
          return $ V.concatMap (.meshes) (snd r)
        Glb path -> do
          let offset' = unit * referenceOffset offset 0 (fromIntegral <$> a)
          V.map (\m -> (m {offset = offset'} :: Mesh)) <$> fromGlb path
        Gltf path -> do
          let offset' = unit * referenceOffset offset 0 (fromIntegral <$> a)
          V.map (\m -> (m {offset = offset'} :: Mesh)) <$> fromGltf path
  where
    colorToV4 t = let (a, b, c, d) = rgba t in V4 a b c d

toMesh :: (HasCallStack, Exception String) => FilePath -> V3 Float -> V2 Float -> (MapDef, Size Int, Vector (V2 Int, ObjectId)) -> IO (Vector Mesh)
toMesh workingDir offset unit (n, size, pos) =
  let unit' = V3 (unit ^. _x) (mapY unit) (unit ^. _y)
   in V.foldMap (parseShape workingDir offset unit' n (both fromIntegral size)) pos

parseMapFile :: (HasCallStack, Exception String) => FilePath -> V3 Float -> V2 Float -> MapFile -> IO (Vector (BB.Box V2 Int, MapDef, ObjectId), Vector SceneObject)
parseMapFile workingDir offset unit m = do
  ds <- either E.throw return (V.mapM (parseMapDef m.size) m.mapData)
  r <-
    V.foldMap
      ( \(a, d) ->
          V.mapM
            ( \(box, _, oid) -> do
                mesh <- toMesh workingDir offset unit (d.defs M.! oid)
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
  return (V.concatMap fst ds, r)

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
          (Cube {..}, _) -> return $ yScale * (yOffset + height)
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
          (Sphere {..}, _) -> return $ yScale * (yOffset + height)
          (Cylinder {..}, _) -> return $ yScale * (yOffset + height)
          (Cone {..}, _) -> return $ yScale * (yOffset + height)
          (Curve {..}, _) -> return $ yScale * (yOffset + height)
          (Reference r y, BB.Box ts te) -> do
            let size = te - ts
                (V2 sx sy) = size
                offset' = referenceOffset offset y ts
            mdef <- loadReferenceDef m.dir r
            let u'' = referenceUnit mdef.size (sx, sy)
                yScale' = mapY u''
            (m', _) <- loadReference offset' 1 (sx, sy) m.dir r
            h <- mapHeight (Maps u'' m' offset' m.size m.dir) t offset'
            return $ yScale' * (h + y)
          (Empty, _) -> return 0
          (Gltf _, _) -> return 0
          (Glb _, _) -> return 0
      )
  return $ if null hs then 0 else V.maximum hs
  where
    center :: V2 Float = start + (end - start) / 2 - m.offset ^. _xz
