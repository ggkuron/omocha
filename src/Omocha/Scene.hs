module Omocha.Scene
  ( Vertex (..),
    Scene (..),
    Mesh (..),
    Topology (..),
    SceneObject (..),
  )
where

import Data.BoundingBox qualified as BB
import Graphics.GPipe
import Omocha.Mesh
import Omocha.Uniform
import RIO

data SceneObject = SceneObject
  { meshes :: Vector Mesh,
    id :: ObjectId,
    bbox :: BB.Box V2 Int
  }
  deriving (Show)

data Scene = Scene
  { objects :: Vector SceneObject,
    camera :: V3 Float
  }
  deriving (Show)
