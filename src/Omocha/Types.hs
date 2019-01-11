{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, FlexibleContexts, RankNTypes #-}

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
    , RenderInput(..)
    , renderScene
    , boardShader
    , light
    , proj
    ) where

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
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Exception (MonadAsyncException)
import Control.Lens


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

-- | Traverse a tree top down accumulating the 'SIDPath' for each node. The projection function enables this to be used on trees that are not 'Scene's.
--   The accumulated 'SIDPath' for a node will include the node's 'SID' at the end, but not its 'ID'. The 'ID' will however be the first 'String' in the
--   'SIDPath's of the children, and the nodes 'SID' won't be included in this case.
topDownSIDPath :: (x -> (SID, Maybe ID)) -> Tree x -> Tree (SIDPath,x)
topDownSIDPath f = topDown g [] 
    where g p x = case f x of
                    (msid, mid) -> let p' = p ++ maybeToList msid
                                   in (maybe p' (:[]) mid, (p', x))

-- | Traverse a tree top down accumulating the absolute transform for each node. The projection function enables this to be used on trees that are not 'Scene's.
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

data RenderInput os = RenderInput {
                        riScreenSize :: V2 Int, 
                        riStream :: PrimitiveArray Triangles (B3 Float, B3 Float, B2 Float),
                        tex :: Texture2D os (Format RGBAFloat)
                      }

type UniInput = (B2 Int32, V3 (B3 Float), B3 Float, B3 Float, B3 Float, B Float)

-- | Gets the transformation matrix of a 'Transform' element.
transformMat :: Transform -> M44 Float
transformMat (LookAt e i u) = lookAt' e i u
transformMat (Matrix m) = m
transformMat (Rotate v a) = rotationVec v (toRadians a)
transformMat (Scale v) = scaling v
transformMat (Skew a r t) = skew (toRadians a) r t
transformMat (Translate v) = translation v

-- | Gets the total transformation matrix of a list of 'Transform' element.
transformsMat :: [Transform] -> M44 Float
transformsMat = foldl !*! identity . map transformMat

-- | The complete transform matrix of all 'Transform' elements in a node.
nodeMat :: Node -> M44 Float
nodeMat = transformsMat . map snd . nodeTransformations

-----------------------------------------

-- | Render the scene using a simple shading technique through the first camera found, or through a defult camera if the scene doesn't contain any cameras. The scene's lights aren't
--   used in the rendering. The source of this function can also serve as an example of how a Collada 'Scene' can be processed.
renderScene :: (MonadIO m, MonadAsyncException m, ContextHandler ctx) => Scene -> (forall os. ContextT ctx os m (V2 Int -> RenderInput os))
renderScene tree = do
    -- Retrieve cameras and geometries tagged with transforms
    let (cameras,geometries) = F.foldMap tagContent $ topDownTransform nodeMat $ fmap snd tree
        tagContent (t,n) = let tagT = zip (repeat t) in (tagT $ nodeCameras n, tagT $ nodeGeometries n) 
    return $ \vpSize -> undefined
    --  where
    --    aspect = fromIntegral w / fromIntegral h
    --    
    --    -- Get the camera and inverted view matrix
    --    (invView,cam) = head (cameras ++ [(translation (V3 0 0 100) , (Nothing, Perspective "" (ViewSizeY 35) (Z 1 10000)))])
    --    
    --    -- The transform matrices
    --    view = fromJust $ invert invView
    --    proj = cameraMat aspect $ snd cam
    --    viewProj = proj `multmm` view
    --          
    --    framebuffer = paint fragmentStream $ newFrameBufferColorDepth (RGB 0) 1
    --    paint = paintColorRastDepth Lequal True NoBlending (RGB $ V3 1 1 1)
    --    -- fragmentStream = fmap (RGB . Vec.vec) $ rasterizeFront primitiveStream
    --    primitiveStream = mconcat $ concatMap filterGeometry geometries
    --    filterGeometry (modelMat, (_,Mesh _ mesh)) = mapMaybe (filterMesh (viewProj `multmm` modelMat) (view `multmm` modelMat) modelMat) mesh
    --    filterMesh modelViewProj modelView modelMat (TriangleMesh _ desc pstream aabb) = do
    --      guard $ hasDynVertex desc "POSITION" (undefined :: V3 Float) -- Filter out geometries without 3D-positions
    --      guard $ hasDynVertex desc "NORMAL" (undefined :: V3 Float)   -- Filter out geometries without 3D-normals
    --      guard $ testAABBprojection modelViewProj aabb /= Outside                -- Frustum cull geometries
    --      let normMat = transpose $ fromJust $ invert $ fmap (V.take n3) $ V.take n3 modelView
    --      return $ fmap (\v -> let p = homPoint $ fromJust $ dynVertex v "POSITION"
    --                               V3 nx ny nz = (toGPU normMat :: M33 Float) * fromJust (dynVertex v "NORMAL")
    --                           in ((toGPU modelViewProj :: M44 Float) * p, maxB nz 0)
    --                    ) pstream
            
boardShader :: ((V3 VFloat, V3 VFloat, V2 VFloat) -> (V3 (S V Float), V3 VFloat, V2 (S V Float))) 
               -> Window os RGBAFloat Depth 
               -> Buffer os (Uniform UniInput) 
               -> Shader os (RenderInput os) ()
boardShader pick win uniform = do
    uni <- getUniform (const (uniform, 0))
    boards <- fmap pick <$> toPrimitiveStream riStream
    let projectedSides = proj uni <$> boards
        filterMode = SamplerFilter Linear Linear Linear (Just 16)
        edge = (pure ClampToEdge, 1.0)
    samp <- newSampler2D $ \ri -> (tex ri, filterMode, edge)

    uv <- rasterize (\ri -> (Front, ViewPort (V2 0 0) (riScreenSize ri), DepthRange 0 1) ) projectedSides
    let litFrags = light samp <$> uv
        litFragsWithDepth = withRasterizedInfo
                               (\p x -> (p, (rasterizedFragCoord x)^._z)) litFrags
        colorOption = ContextColorOption (BlendRgbAlpha (FuncAdd, FuncAdd) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors Zero One) (V4 0 0 0 0)) (pure True)
        depthOption = DepthOption Lequal True
    
    drawWindowColorDepth (const (win, colorOption, depthOption)) litFragsWithDepth


light :: ColorSampleable c => Sampler2D (Format c) -> (t, V2 (S F Float)) -> ColorSample F c
light samp (normal, uv) = sample2D samp SampleAuto (Just 1) Nothing uv 

proj uni (V3 px py pz, normal, uv) =   
    let modelViewProj = perspective (pi/3) (let V2 w h = windowSize uni in (toFloat w) / (toFloat h)) 1 (-1)
        normMat = modelNorm uni
        viewProj = lookAt' (viewCamera uni) (viewTarget uni) (viewUp uni)
        in (modelViewProj !*! viewProj !* V4 px py pz 1, (fmap Flat $ normMat !* normal, uv))   

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


