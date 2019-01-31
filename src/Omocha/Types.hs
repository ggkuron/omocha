{-# LANGUAGE StandaloneDeriving
 , DeriveDataTypeable
 , FlexibleContexts
 , RankNTypes 
 , ScopedTypeVariables 
 , RecordWildCards
 , GADTs
 #-}

module Omocha.Types (
    ID
    , SID
    , Semantic
    , ColladaTree(..)
    , Transform(..)
    , ColladaNode(..)
    , Camera(..)
    , ViewSize(..)
    , Z(..)
    , Light(..)
    , Geometry(..)
    , AABB(..)
    , ColladaMesh(..)
    , ColladaMeshPrimitive(..)
    , ColladaMeshPrimitiveArray(..)
    , Attenuation(..)
    , RenderInput(..)
    , renderColladaTree
    , UniInput
    , boardShader
    , monoShader
    , light
    , windowSize
    , modelNorm 
    , viewCamera
    , viewTarget
    , viewUp    
    , VBuffer
    , DrawVertex(..)
    , OmochaShaderType(..)
    , Scene(..)
    , Mesh(..)
    , TextureInput
    , PlainInput
    ) where


import Omocha.Bitmap
import Data.Tree as Tree
import Data.Tree (Tree(), Forest)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Dynamic
import Data.Typeable
import Data.Maybe
import Data.Int (Int32)
import qualified Data.Vector as V 
import qualified Linear.V as V
import qualified Data.Foldable as F
import Graphics.GPipe 
import Linear.Matrix
import Linear.Vector
import Linear.Metric
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Exception (MonadAsyncException)
import Control.Lens



type ID = String
type SID = Maybe String
type Semantic = String


type ColladaTree = Tree (SID, ColladaNode)
type ColladaColor = V3 Float

data DrawVertex = DrawVertex {
                    dvPosition :: V3 Float,
                    dvNormal :: V3 Float,
                    dvUv :: V2 Float
                } deriving (Show)


data Mesh = Mesh {
              vertices :: [DrawVertex],
              indices :: Maybe [Int],
              offset :: V3 Float,
              textureImage :: Maybe Bitmap,
              shaderType :: OmochaShaderType
          }

instance Show Mesh where
    show Mesh{..} = show vertices ++ show indices ++ show shaderType

data OmochaShaderType = BoardShader | TargetBoard
    deriving (Eq, Ord, Show)


data Scene = Scene {
               meshes :: [Mesh],
               camera :: V3 Float
           } deriving (Show)



data ColladaNode = ColladaNode {
    nodeId:: Maybe ID,
    nodeLayers :: [String],
    nodeTransformations :: [(SID, Transform)],
    nodeCameras :: [(SID, Camera)],
    nodeLights :: [(SID, Light)],
    nodeGeometries :: [(SID, Geometry)]
} 

instance Show ColladaNode where
    show ColladaNode{..} = show nodeId ++ show nodeLayers ++ show nodeGeometries


data Transform = LookAt {
                    lookAtEye:: V3 Float,
                    lookAtInterest:: V3 Float,
                    lookAtUp :: V3 Float
                 }
               | Matrix (M44 Float)
               | Rotate (V3 Float) Float
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


data Geometry =  ColladaMesh {
    meshID :: ID,
    meshPrimitives :: [ColladaMesh]
} deriving (Show)

deriving instance Show Triangles
deriving instance Show a => Show (PrimitiveTopology a)

data ColladaMesh = TriangleColladaMesh {
    meshMaterial :: String,
    meshDescription :: Map Semantic TypeRep,
    meshPrimitiveStream :: ColladaMeshPrimitiveArray (PrimitiveTopology Triangles) (Map Semantic Dynamic),
    meshAABB :: AABB
} deriving (Show)

data ColladaMeshPrimitive p a = ColladaMeshPrimitive p a
                       | ColladaMeshPrimitiveIndexed p [Int] a
        deriving(Show)

newtype ColladaMeshPrimitiveArray p a = ColladaMeshPrimitiveArray { getColladaMeshPrimitiveArray :: [ColladaMeshPrimitive p a]}
    deriving (Show)

instance Monoid (ColladaMeshPrimitiveArray p a) where
    mempty = ColladaMeshPrimitiveArray []
    mappend (ColladaMeshPrimitiveArray a) (ColladaMeshPrimitiveArray b) = ColladaMeshPrimitiveArray (a ++ b)
instance Functor (ColladaMeshPrimitiveArray p) where
    fmap f (ColladaMeshPrimitiveArray xs) = ColladaMeshPrimitiveArray $ fmap g xs
        where g (ColladaMeshPrimitive p a) = ColladaMeshPrimitive p (f a)
              g (ColladaMeshPrimitiveIndexed p i a) = ColladaMeshPrimitiveIndexed p i (f a)

data AABB = AABB {
                aabbMin :: V3 Float,
                aabbMax :: V3 Float
            } deriving (Show, Eq)

instance Monoid AABB where
    mempty = let inf = read "Infinity" :: Float in AABB (V3 inf inf inf) (V3 (-inf) (-inf) (-inf))
    mappend (AABB minA maxA) (AABB minB maxB) = AABB (zipAsVector min minA minB) (zipAsVector max maxA maxB)
        where 
        zipAsVector minMax a b =  V.fromV . fromJust . V.fromVector $ V.zipWith minMax (V.toVector $ V.toV a) (V.toVector $ V.toV b)


-- | Traverse a Tree top down, much like 'mapAccumL'. The function recieves an accumulator from its parent and generates one that is passed down to its children.
--   Useful for accumulating transformations, etc.
topDown :: (acc -> x -> (acc, y)) -> acc -> Tree x -> Tree y
topDown f a (Tree.Node x xs) = case f a x of (acc', y) -> Tree.Node y $ map (topDown f acc') xs

-- | Traverse a Tree bottom up, much like 'mapAccumR'. The function recieves the accumulators of all its children and generates one that is passed up to its parent.
--   Useful for accumulating AABBs, etc.
bottomUp :: ([acc] -> x -> (acc, y)) -> Tree x -> (acc, Tree y)
bottomUp f (Tree.Node x xs) = case unzip $ map (bottomUp f) xs of (accs, ys) -> case f accs x of (acc', y) -> (acc', Tree.Node y ys)

-- | Remove branches of a tree where the function evaluates to 'True'. Useful for selecting LODs, etc.
prune :: (a -> Bool) -> Tree a -> Maybe (Tree a)
prune f (Tree.Node x xs) = if f x then Nothing else Just $ Tree.Node x $ mapMaybe (prune f) xs


---------------------------------
-- | A path where the first string in the list is the closest ID and the rest is all SIDs found in order.
type SIDPath = [String]

-- | Traverse a tree top down accumulating the 'SIDPath' for each node. The projection function enables this to be used on trees that are not 'ColladaTree's.
--   The accumulated 'SIDPath' for a node will include the node's 'SID' at the end, but not its 'ID'. The 'ID' will however be the first 'String' in the
--   'SIDPath's of the children, and the nodes 'SID' won't be included in this case.
topDownSIDPath :: (x -> (SID, Maybe ID)) -> Tree x -> Tree (SIDPath,x)
topDownSIDPath f = topDown g [] 
    where g p x = case f x of
                    (msid, mid) -> let p' = p ++ maybeToList msid
                                   in (maybe p' (:[]) mid, (p', x))

-- | Traverse a tree top down accumulating the absolute transform for each node. The projection function enables this to be used on trees that are not 'ColladaTree's.
--   Use 'nodeMat' to get the @Mat44 Float@ from a node. The accumulated matrix for each node will contain the node's own transforms.
topDownTransform :: (x -> M44 Float) -> Tree x -> Tree (M44 Float,x)
topDownTransform f = topDown g identity
    where g t x = let t' = t * f x in (t', (t', x))

-- | For each 'SID'-attributed element in a list, apply the provided function. The elements where the function evaluates to 'Nothing' are removed.
alterSID :: (String -> a -> Maybe a) -> [(SID,a)] -> [(SID,a)]
alterSID f = mapMaybe (alterSID' f)
    where
        alterSID' f (msid@(Just sid), x) = fmap ((,) msid) $ f sid x
        alterSID' _ a = Just a
        
-- | Find the first element in the list that is attributed with the provided SID.
lookupSID :: String -> [(SID,a)] -> Maybe a
lookupSID = lookup . Just

-- | Evaluates to 'True' where the 'Map' contains the semantic with the same type as the last argument (which is not evaluated and prefferably is a monotyped 'undefined').
hasDynVertex :: Typeable a => Map Semantic TypeRep -> Semantic -> a -> Bool
hasDynVertex m s a = maybe False (== typeOf a) $ Map.lookup s m

-- | Extract the value of a specific semantic from the 'Map'. If the semantic is not found or the type is wrong, it evaluates to 'Nothing'.
dynVertex :: Typeable a => Map Semantic Dynamic -> Semantic -> Maybe a
dynVertex m s = Map.lookup s m >>= fromDynamic


type VBuffer = (B3 Float, B3 Float, B2 Float)

data TextureInput
data PlainInput

data RenderInput os tag where
    RenderInput :: V2 Int -> PrimitiveArray Triangles VBuffer -> Texture2D os (Format RGBAFloat) -> RenderInput os TextureInput
    PlainInput :: V2 Int -> PrimitiveArray Triangles VBuffer -> RenderInput os PlainInput

riScreenSize :: RenderInput os tag -> V2 Int
riScreenSize (RenderInput vp _ _) = vp
riScreenSize (PlainInput vp _) = vp
riStream :: RenderInput os tag -> PrimitiveArray Triangles VBuffer
riStream (RenderInput _ st _) = st
riStream (PlainInput _ st) = st
tex :: RenderInput os TextureInput -> Texture2D os (Format RGBAFloat)
tex (RenderInput _ _ tex) = tex



type UniInput = (B2 Int32, V3 (B3 Float), B3 Float, B3 Float, B3 Float, B Float)

windowSize  (a, _, _, _, _, _) = a
modelNorm   (_, a, _, _, _, _) = a
viewCamera  (_, _, a, _, _, _) = a 
viewTarget  (_, _, _, a, _, _) = a
viewUp      (_, _, _, _, a, _) = a


toRadians :: Floating a => a -> a
toRadians d = d * pi / 180

-- | Gets the transformation matrix of a 'Transform' element.
transformMat :: Transform -> M44 Float
transformMat (LookAt e i u) = lookAt e i u
transformMat (Matrix m) = m
transformMat (Rotate v a) = mkTransformation (axisAngle v (toRadians a)) v
transformMat (Scale v) = m33_to_m44 $ scaled v
transformMat (Skew a r t) = skew (toRadians a) r t
transformMat (Translate v) = V4 (V4 1 0 0 (v^._x))
                                (V4 0 1 0 (v^._y))
                                (V4 0 0 1 (v^._z))
                                (V4 0 0 0 1)
                              

-- | Gets the total transformation matrix of a list of 'Transform' element.
transformsMat :: [Transform] -> M44 Float
transformsMat = foldl (!*!) identity . map transformMat

-- | The complete transform matrix of all 'Transform' elements in a node.
nodeMat :: ColladaNode -> M44 Float
nodeMat = transformsMat . map snd . nodeTransformations

-- adopted from http://www.koders.com/cpp/fidA08C276050F880D11C2E49280DD9997478DC5BA1.aspx
skew :: Float -> V3 Float -> V3 Float -> M44 Float
skew angle a b = m33_to_m44 m
    where
        n2 :: V3 Float
        n2 = normalize b
        a1 = fmap (* (a `dot` n2)) n2
        a2 = a-a1
        n1 = normalize a2
        an1 = a `dot` n1
        an2 = a `dot` n2
        rx = an1 * cos angle - an2 * sin angle
        ry = an1 * sin angle + an2 * cos angle
        alpha = if abs an1 < 0.000001 then 0 else ry/rx-an2/an1
        n3 :: V3 Float
        n3 = n2 ^* alpha
        m = n1 `outer` n3 + identity
        
-----------------------------------------
--
foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f = go where
    go (Tree.Node x ts) = f x (map go ts)

-- | Render the scene using a simple shading technique through the first camera found, or through a defult camera if the scene doesn't contain any cameras. The scene's lights aren't
--   used in the rendering. The source of this function can also serve as an example of how a Collada 'ColladaTree' can be processed.
-- \root@(sid, node) fs -> 
renderColladaTree :: ColladaTree -> Scene
renderColladaTree tree = 
    let (cameras, geometries) = F.foldMap tagContent $ topDownTransform nodeMat $ fmap snd tree
        primitiveStream = mconcat $ concatMap filterGeometry geometries :: ColladaMeshPrimitiveArray (PrimitiveTopology Triangles) ([V3 Float], [V3 Float])

    in Scene {
        camera = V3 0 0 0,
        meshes = fmap toMesh (getColladaMeshPrimitiveArray primitiveStream)
    }
    where
      toMesh :: ColladaMeshPrimitive (PrimitiveTopology Triangles) ([V3 Float], [V3 Float]) -> Mesh
      toMesh (ColladaMeshPrimitive _ vertices) = Mesh [DrawVertex v n (V2 0 0) | (v, n) <- zip (fst vertices) (snd vertices) ] Nothing (V3 0 0 0) Nothing BoardShader
      toMesh (ColladaMeshPrimitiveIndexed _ indices vertices) = Mesh [ DrawVertex v n (V2 0 0) | (v, n) <- zip (fst vertices) (snd vertices)] (Just indices) (V3 0 0 0) Nothing BoardShader
      tagT t = zip (repeat t)
      tagContent (t, n) = (tagT t $ nodeCameras n, tagT t $ nodeGeometries n) 
      filterGeometry (modelMat, (_,ColladaMesh _ mesh)) = mapMaybe (filterColladaMesh modelMat) mesh
      filterColladaMesh modelMat (TriangleColladaMesh _ desc pstream aabb) = do
        guard $ hasDynVertex desc "POSITION" (undefined :: V3 Float) -- Filter out geometries without 3D-positions
        guard $ hasDynVertex desc "NORMAL" (undefined :: V3 Float)   -- Filter out geometries without 3D-normals
        -- guard $ testAABBprojection modelViewProj aabb /= Outside                -- Frustum cull geometries
        return $ fmap (\v -> let p = fromJust $ dynVertex v "POSITION"
                                 n = fromJust $ dynVertex v "NORMAL"
                             in (p, n)
                      ) pstream
            

boardShader :: ((UniformFormat UniInput V) 
                -> (V3 VFloat, V3 VFloat, V2 VFloat) 
                -> (V3 (S V Float), V3 VFloat, V2 (S V Float))
               )-> Window os RGBAFloat Depth 
                -> Buffer os (Uniform UniInput) 
                -> Shader os (RenderInput os TextureInput) ()
boardShader pick win uniform = do
    uni <- getUniform (const (uniform, 0))
    boards <- fmap (pick uni) <$> toPrimitiveStream riStream
    let projected = fmap (\(v, n, uv) -> let (v', n') = proj uni (v, n) in (v', (n', uv))) boards
        filterMode = SamplerFilter Linear Linear Linear (Just 16)
        edge = (pure ClampToEdge, 1.0)
    samp <- newSampler2D $ \ri -> (tex ri, filterMode, edge)

    fragmentStream <- rasterize (\ri -> (Front, ViewPort (V2 0 0) (riScreenSize ri), DepthRange 0 1) ) projected
    let litFrags = light samp <$> fragmentStream
        litFragsWithDepth = withRasterizedInfo
                               (\p x -> (p, (rasterizedFragCoord x)^._z)) litFrags
        colorOption = ContextColorOption (BlendRgbAlpha (FuncAdd, FuncAdd) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors Zero One) (V4 0 0 0 0)) (pure True)
        depthOption = DepthOption Lequal True
    
    drawWindowColorDepth (const (win, colorOption, depthOption)) litFragsWithDepth

monoShader :: ((UniformFormat UniInput V) 
               -> (V3 VFloat, V3 VFloat, V2 VFloat) 
               -> (V3 (S V Float), V3 VFloat, V2 (S V Float))
              )-> Window os RGBAFloat Depth 
               -> Buffer os (Uniform UniInput) 
               -> Shader os (RenderInput os PlainInput) ()
monoShader pick win uniform = do
    uni <- getUniform (const (uniform, 0))
    boards <- fmap (pick uni) <$> toPrimitiveStream riStream
    let projected = fmap (\(v, n, _) -> let (v', n') = proj uni (v, n) in (v',  V4 1 0 0 (0.5) :: V4 VFloat)) boards
    fragmentStream <- rasterize (\ri -> (Front, ViewPort (V2 0 0) (riScreenSize ri), DepthRange 0 1) ) projected
    let litFrags = fragmentStream
        litFragsWithDepth = withRasterizedInfo
                               (\p x -> (p, (rasterizedFragCoord x)^._z)) litFrags
    let colorOption = ContextColorOption (BlendRgbAlpha (FuncAdd, FuncAdd) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors Zero One) (V4 0 0 0 0)) (pure True)
        depthOption = DepthOption Lequal True
    
    drawWindowColorDepth (const (win, colorOption, depthOption)) litFragsWithDepth


light :: ColorSampleable c => Sampler2D (Format c) -> (t, V2 (S F Float)) -> ColorSample F c
light samp (normal, uv) = sample2D samp SampleAuto (Just 1) Nothing uv 


proj :: (Functor f, Floating (ConvertFloat a), Convert a, Foldable r, Additive r) =>
        (V2 a, f (r VFloat), V3 (ConvertFloat a), V3 (ConvertFloat a), V3 (ConvertFloat a), t)
        -> (V3 (ConvertFloat a), r VFloat)
        -> (V4 (ConvertFloat a), f FlatVFloat)
proj uni (V3 px py pz, normal) =   
    let modelViewProj = perspective (pi/3) (let V2 w h = windowSize uni in (toFloat w) / (toFloat h)) 1 (-1)
        normMat = modelNorm uni
        viewProj = lookAt' (viewCamera uni) (viewTarget uni) (viewUp uni)
    in (modelViewProj !*! viewProj !* V4 px py pz 1, (fmap Flat $ normMat !* normal))   

lookAt' eye center up =
    V4 (V4 (xa^._x)  (xa^._y)  (xa^._z)  xd)
            (V4 (ya^._x)  (ya^._y)  (ya^._z)  yd)
            (V4 (-za^._x) (-za^._y) (-za^._z) zd)
            (V4 0         0         0          1)
         where za = signorm $ center - eye
               xa = signorm $ cross za up
               ya = cross xa za
               xd = -dot xa eye
               yd = -dot ya eye
               zd = dot za eye
     
