module Omocha.Scene
  ( Vertex (..),
    Scene (..),
    Mesh (..),
    Topology (..),
    ObjectId (..),
    SceneObject (..),
    OmochaShaderType (..),
  )
where

import Graphics.GPipe
import Omocha.Bitmap
import RIO

data Vertex = Vertex
  { position :: V3 Float,
    normal :: V3 Float,
    uv :: V2 Float
  }
  deriving (Show)

newtype ObjectId = ObjectId Int
  deriving (Eq, Ord, Show)

data SceneObject = SceneObject
  { meshes :: Vector Mesh,
    id :: ObjectId
  }
  deriving (Show)

data OmochaShaderType = BoardShader | TargetBoard
  deriving (Eq, Ord, Show)

data Mesh = Mesh
  { vertices :: [Vertex],
    indices :: Maybe [Int],
    offset :: V3 Float,
    texture :: Maybe Bitmap,
    shader :: OmochaShaderType,
    topology :: Topology,
    color :: Maybe (V4 Float)
  }

instance Show Mesh where
  show Mesh {..} = show vertices ++ show indices ++ show shader

data Scene = Scene
  { objects :: Vector SceneObject,
    camera :: V3 Float
  }
  deriving (Show)

data Topology where
  TopologyTriangles :: PrimitiveTopology Triangles -> Topology
  TopologyLines :: PrimitiveTopology Lines -> Topology
  TopologyPoints :: PrimitiveTopology Points -> Topology

instance Show Topology where
  show (TopologyTriangles _) = "Triangles"
  show (TopologyLines _) = "Lines"
  show (TopologyPoints _) = "Points"
