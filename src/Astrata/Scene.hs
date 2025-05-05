module Astrata.Scene
  ( Vertex (..),
    Scene (..),
    Mesh (..),
    Topology (..),
    SceneObject (..),
  )
where

import Astrata.Mesh
import Astrata.Uniform
import Graphics.GPipe
import RIO

data SceneObject = SceneObject
  { meshes :: Vector Mesh,
    id :: ObjectId
  }
  deriving (Show)

data Scene = Scene
  { objects :: Vector SceneObject,
    camera :: V3 Float
  }
  deriving (Show)
