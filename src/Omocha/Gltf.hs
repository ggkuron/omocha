module Omocha.Gltf where

import Control.Monad
import Control.Monad.Exception qualified as E (MonadException (throw), throw)
import Data.Vector qualified as V
import Graphics.GPipe
import Omocha.Mesh
import RIO hiding (trace, traceStack)
import Text.GLTF.Loader (Gltf (gltfMeshes))
import Text.GLTF.Loader qualified as GLTF
import Prelude (userError)

meshFromGltf :: GLTF.Gltf -> Vector Mesh
meshFromGltf j = V.concatMap (\(GLTF.Mesh _meshName prims _wieghts) -> processMeshPrimitive <$> prims) (gltfMeshes j)
  where
    processMeshPrimitive :: GLTF.MeshPrimitive -> Mesh
    processMeshPrimitive (GLTF.MeshPrimitive indices _material mode normals positions texcoords) =
      Mesh
        { vertices = V.toList $ V.zipWith3 Vertex positions normals texcoords,
          indices = Just . V.toList $ V.map fromIntegral indices,
          offset = V3 1 0 0,
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

fromGltf :: FilePath -> IO (Vector Mesh)
fromGltf path = do
  json <- liftIO $ GLTF.fromJsonFile path
  j <- liftIO $ case json of
    Left e -> E.throw . userError $ show e
    Right v' -> return v'
  return $ meshFromGltf j

fromGlb :: FilePath -> IO (Vector Mesh)
fromGlb path = do
  glb <- liftIO $ GLTF.fromBinaryFile path
  j <- liftIO $ case glb of
    Left e -> E.throw . userError $ show e
    Right v' -> return v'
  return $ meshFromGltf (GLTF.unGltf j)
