{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, FlexibleContexts #-}

module Omocha.Types (
    ID
    , SID
    , Semantic
    , Scene(..)
    , Transform(..)
    , Node(..)
    , Camera(..)
    , ViewSize(..)
    , Z(..)
    , Light(..)
    , Geometry(..)
    , AABB(..)
    , Mesh(..)
    , MeshPrimitive(..)
    , MeshPrimitiveArray(..)
    , Attenuation(..)
    ) where

import Data.Tree
import Data.Map (Map)
import Data.Dynamic
import Data.Typeable
import Data.Maybe
import qualified Data.Vector as V 
import qualified Linear.V as V
import Graphics.GPipe 
import Linear.Matrix

type ID = String
type SID = Maybe String
type Semantic = String

type Scene = Tree (SID, Node)
type ColladaColor = V3 Float

data Node = Node {
    nodeId:: Maybe ID,
    nodeLayers :: [String],
    nodeTransformations :: [(SID, Transform)],
    nodeCameras :: [(SID, Camera)],
    nodeLights :: [(SID, Light)],
    nodeGeometries :: [(SID, Geometry)]
} 



data Transform = LookAt {
                    lookAtEye:: V3 Float,
                    lookAtInterest:: V3 Float,
                    lookAtUp :: V3 Float
                 }
               | Matrix (M44 Float)
               | Rotate (M44 Float) Float
               | Scale (V3 Float) 
               | Skew {
                    skewAngle:: Float,
                    skewRotation:: V3 Float,
                    skewTranslation :: V3 Float
                 }
               | Translate (V3 Float)
               deriving (Show, Eq)


data Camera = Perspective {
                perspectiveID :: ID,
                perspectiveFov :: ViewSize,
                perspectiveZ :: Z
              }
            | Orthographic {
                orthographicID :: ID,
                orthographicViewSize :: ViewSize,
                orthographicZ :: Z
              }
              deriving (Show, Eq)


data ViewSize = ViewSizeX Float
              | ViewSizeY Float
              | ViewSizeXY (V2 Float)
              deriving (Show, Eq)

data Z = Z {
             zNear :: Float,
             zFar :: Float
           } 
           deriving (Show, Eq)


data Light = Ambient {
                ambientID :: ID,
                ambientColladaColor :: ColladaColor
             }
           | Directional {
               directionalID :: ID,
               directionalColladaColor :: ColladaColor
             }
           | Point {
               pointID :: ID,
               pointColladaColor :: ColladaColor,
               pointAttenuation :: Attenuation
             }
           | Spot {
               spotID :: ID,
               spotColladaColor :: ColladaColor,
               spotAttenuation :: Attenuation,
               spotFallOffAngle :: Float,
               spotFallOffExponent :: Float
             } 


data Attenuation = Attenuation {
        attenuationConstant:: Float,
        attenuationLinear :: Float,
        attenuationQuandratic :: Float
    } deriving (Show, Eq)


data Geometry =  Mesh {
    meshID :: ID,
    meshPrimitives :: [Mesh]
} 

data Mesh = TriangleMesh {
    meshMaterial :: String,
    meshDescription :: Map Semantic TypeRep,
    meshPrimitiveStream :: MeshPrimitiveArray (PrimitiveTopology Triangles) (Map Semantic Dynamic),
    meshAABB :: AABB
} 

data MeshPrimitive p a = MeshPrimitive p a
                       | MeshPrimitiveIndexed p [Int] a

newtype MeshPrimitiveArray p a = MeshPrimitiveArray { getMeshPrimitiveArray :: [MeshPrimitive p a]}

instance Monoid (MeshPrimitiveArray p a) where
    mempty = MeshPrimitiveArray []
    mappend (MeshPrimitiveArray a) (MeshPrimitiveArray b) = MeshPrimitiveArray (a ++ b)
instance Functor (MeshPrimitiveArray p) where
    fmap f (MeshPrimitiveArray xs) = MeshPrimitiveArray $ fmap g xs
        where g (MeshPrimitive p a) = MeshPrimitive p (f a)
              g (MeshPrimitiveIndexed p i a) = MeshPrimitiveIndexed p i (f a)

data AABB = AABB {
                aabbMin :: V3 Float,
                aabbMax :: V3 Float
            }
            deriving (Show, Eq)

instance Monoid AABB where
    mempty = let inf = read "Infinity" :: Float in AABB (V3 inf inf inf) (V3 (-inf) (-inf) (-inf))
    mappend (AABB minA maxA) (AABB minB maxB) = AABB (zipAsVector min minA minB) (zipAsVector max maxA maxB)
        where 
        zipAsVector minMax a b =  V.fromV . fromJust . V.fromVector $ V.zipWith minMax (V.toVector $ V.toV a) (V.toVector $ V.toV b)

    
