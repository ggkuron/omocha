{-# LANGUAGE RankNTypes
  , TypeFamilies
  , StandaloneDeriving
  , DeriveDataTypeable
  , FlexibleContexts 
 , RecordWildCards
#-}
module Omocha.Collada (
    readCollada
    , readColladaFile
    , SID
    , ID
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
    , sceneFromCollada
) where

import Graphics.GPipe 

import Data.Tree (Tree(), Forest)
import qualified Data.Tree as Tree
import Data.Array
import Data.Map (Map)
import Data.Function
import Data.Maybe (fromJust, mapMaybe, catMaybes, maybeToList)
import Data.List hiding (union, transpose)
import qualified Data.Map as Map
import qualified Data.Foldable as F
import Data.Vector (Vector) 
import qualified Data.Vector as V 
import qualified Linear.Vector as V
import qualified Linear.V as V
import Linear.Matrix hiding (transpose)
import Text.XML.HaXml hiding (Document, when, Reference, (!))
import qualified Text.XML.HaXml as XML
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn

import Data.Typeable
import Data.Dynamic

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Error (strMsg)
import Control.Monad.Writer.Strict
import Control.Arrow (first, second)
import Control.Lens hiding(children, deep)
import Omocha.Scene

import System.IO.Unsafe(unsafePerformIO)
import Unsafe.Coerce(unsafeCoerce)

type ID = String
type SID = Maybe String
type Semantic = String


type ColladaTree = Tree (SID, ColladaNode)
type ColladaColor = V3 Float

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




toRadians :: Floating a => a -> a
toRadians d = d * pi / 180


newtype RefMap = RefMap (Map ID (RefMap -> Reference))

data Reference = RefNode ColladaTree
               | RefArray (Maybe ([Float], Int))
               | RefSource (Maybe ([[Float]], Int))
               | RefVertices (Map String ([[Float]], Int))
               | RefVisualColladaTree ColladaTree
               | RefCamera Camera
               | RefLight Light
               | RefGeometry Geometry



type Parser = WriterT [(ID, RefMap -> Reference)] (WriterT [RefMap -> Either String ()] (Either String))


runParser :: Parser (Maybe ID) -> Either String Reference
runParser m = do ((mid, refs), checks) <- runWriterT $ runWriterT m
                 case mid of
                    Nothing -> return $ RefVisualColladaTree $ Tree.Node (nosid $ ColladaNode Nothing [] [] [] [] []) []
                    Just id -> do
                        let refmap = RefMap $ Map.fromList refs
                        mapM_ ($ refmap) checks
                        return $ fromJust $ getRef id refmap


addRefF id ref = tell [(id, ref)]
getRef id a@(RefMap m) = fmap ($a) $ Map.lookup id m

assert f = lift $ tell [f]


readCollada f s = do p <- xmlParse' f s
                     xs <- withError "Expecting COLLADA top-element" $ do XML.Document _ _ (Elem (N "COLLADA") _ xs) _ <- return p
                                                                          return xs

                     RefVisualColladaTree vs <- runParser $ parseDoc xs
                     return vs

                                                                        
readColladaFile :: FilePath -> IO ColladaTree
readColladaFile f = readFile f >>= (\s -> case readCollada f s of
                                            Left e -> throwError $ strMsg e
                                            Right v -> return v)
                 

missingLinkErr el id c = el ++ " element with id '" ++ id ++ "' not found when processing " ++ errorPos c

errorPos (CElem (Elem n _ _) p) = show n ++ " element in " ++ show p ++ "."
errorPos _ = "unknown position"

sid s a = (Just s, a)
nosid a = (Nothing, a)

localUrl ('#':id) = Just id
localUrl _ = Nothing

withError err m = m `mplus` throwError err

makeSID "" = nosid
makeSID s = sid s

changeTreeSID "" (Tree.Node (_, node) xs) = Tree.Node (nosid node) xs
changeTreeSID s (Tree.Node (_, node) xs) = Tree.Node (sid s node) xs

getAttribute s = fst . head . attributed s keep
getReqAttribute :: String -> Content Posn -> Parser String
getReqAttribute s c = case getAttribute s c of
                        "" -> throwError $ "Missing attribute " ++ s ++ " in " ++ errorPos c
                        a -> return a

getStringContent = unwords . mapMaybe fst . textlabelled children

getReqSingleElement el c = case c -=> keep /> tag el of
                            [] -> throwError $ "Missing " ++ el ++ " element in " ++ errorPos c
                            [e] -> return e
                            _ -> throwError $ "Multiple " ++ el ++ " elements in " ++ errorPos c

getFromReqAttribute attr c = do a <- getReqAttribute attr c
                                fromString ("Malformed " ++ attr ++ " attribute in " ++ errorPos c) a

getFromAttributeDef attr def c = do let a = getAttribute attr c
                                    if null a
                                        then return def
                                        else fromString ("Malformed " ++ attr ++ " attribute in " ++ errorPos c) a

getFromSingleElementDef el def c = case c -=> keep /> tag el of
                                    [] -> return def
                                    [e] -> getFromContents e
                                    _ -> throwError $ "Multiple " ++ el ++ " elements in " ++ errorPos c

getFromListContents c = fromList ("Malformed contents of " ++ errorPos c) $ getStringContent c
getFromContents c = fromString ("Malformed contents of " ++ errorPos c) $ getStringContent c

getFromListLengthContents n c = do xs <- getFromListContents c
                                   if length xs == n then return xs else
                                      if length xs < n 
                                          then throwError $ "Too few elements in " ++ errorPos c 
                                          else throwError $ "Too many elements in " ++ errorPos c

fromList err = mapM (fromString err) . words
fromString err = parse . reads
    where parse [(a,"")] = return a
          parse _ = throwError err

fromListToVec = V.fromV . fromJust . V.fromVector . V.fromList

fromListToMat44 xs = V4 (V4 (xs!!0)  (xs!!1)  (xs!!2)  (xs!!3))
                        (V4 (xs!!4)  (xs!!5)  (xs!!6)  (xs!!7))
                        (V4 (xs!!8)  (xs!!9)  (xs!!10) (xs!!11))
                        (V4 (xs!!12) (xs!!13) (xs!!14) (xs!!15))

infixl 2 ==>, -=>
xs ==> f = concatMap f xs
x -=> f = f x



parseDoc xs = do let sources = xs ==> deep (tagWith (`elem` ["animation", "mesh", "morph", "skin", "spline", "convex_mesh", "brep", "nurbs", "nurbs_surface"])) /> tag "source"
                 mapM_ parseArray $ sources ==> tagged (keep /> tagWith ("_array" `isSuffixOf`) `with` attr "id")
                 mapM_ parseSource sources
                 mapM_ parseCamera $ xs ==> tag "library_cameras" /> tag "camera" `with` attr "id"
                 mapM_ parseGeometry $ xs ==> tag "library_geometries" /> tag "geometry"
                 mapM_ parseLight $ xs ==> tag "library_lights" /> tag "light" `with` attr "id"
                 mapM_ parseNode $ xs ==> tag "library_nodes" /> tag "node"
                 mapM_ parseVisualColladaTree $ xs ==> tag "library_visual_scenes" /> tag "visual_scene"
                 (url,c) <- case xs ==> attributed "url" (tag "scene" /> tag "instance_visual_scene") of
                       [] -> throwError "Missing scene element with instance_visual_scene element found in COLLADA top element."
                       [x] -> return x
                       _ -> throwError "Multiple instance_visual_scene elements in scene element found in COLLADA top element."
                 case localUrl url of
                        Nothing -> return Nothing
                        Just lurl -> do assert $ \refmap -> withError (missingLinkErr "visual_scene" lurl c) $ do Just (RefVisualColladaTree _) <- return $ getRef lurl refmap
                                                                                                                  return ()
                                        return $ Just lurl

---------------------------------------------------------------


parseArray (s, arr) = do arrRef <- case s of 
                                    "float_array" -> do
                                        count <- getFromReqAttribute "count" arr
                                        xs <- getFromListContents arr
                                        let len = length xs
                                        when (len /= count)
                                            $ throwError $ "Length of array not the same as the value of count attribute in " ++ errorPos arr
                                        return $ Just (xs :: [Float], len)
                                    _ -> return Nothing
                         addRefF (getAttribute "id" arr) (const (RefArray arrRef))

---------------------------------------------------------------
          
parseSource s = do id <- getReqAttribute "id" s
                   sRef <- case s -=> keep /> tag "technique_common" of
                                [] -> return (const (RefSource Nothing))
                                tc:_ -> do acc <- getReqSingleElement "accessor" tc
                                           parseAccessor acc
                   addRefF id sRef

parseAccessor acc = do arrUrl <- getReqAttribute "source" acc
                       case localUrl arrUrl of
                            Nothing -> return (const (RefSource Nothing))
                            Just id -> do
                               count <- getFromReqAttribute "count" acc
                               offset <- getFromAttributeDef "offset" 0 acc
                               stride <- getFromAttributeDef "stride" 1 acc
                               useParamList <- mapM parseParam $ acc -=> keep /> elm
                               let paramLength = length useParamList
                               when (paramLength > stride)
                                   $ throwError $ "stride attribute too low in " ++ errorPos acc                              
                               let requiredLength = offset + stride * (count-1) + paramLength
                               assert $ \refmap -> do m <- withError (missingLinkErr "*_array" id acc) $ do Just (RefArray m) <- return $ getRef id refmap
                                                                                                            return m
                                                      case m of Just (_,len) | requiredLength > len -> throwError $ "Source size too small for " ++ errorPos acc 
                                                                _ -> return ()
                               return $ \refmap -> RefSource $ case getRef id refmap of
                                                                    Just (RefArray (Just (source, len))) -> Just (assembleSource (drop offset source) count stride useParamList, count)
                                                                    _ -> Nothing
    where parseParam c | null $ tag "param" c = throwError $ "Unexpected " ++ errorPos c
                       | otherwise = return $ not $ null $ getAttribute "name" c
          assembleSource source 0 _ _ = []
          assembleSource source count stride useParamList = case splitAt stride source of (vertex, rest) -> map snd (filter fst (zip useParamList vertex)) : assembleSource rest (count - 1) stride useParamList


---------------------------------------------------------------

parseNode c | not $ null $ tag "node" c = do
                    let id = getAttribute "id" c
                        mid = if id == "" then Nothing else Just id
                        sid = makeSID $ getAttribute "sid" c
                        layer = words $ getAttribute "layer" c
                    transformations <- fmap catMaybes $ mapM parseTransformations $ children c
                    cameraFs <- fmap catMaybes $ mapM parseCameraInstances $ c -=> keep /> tag "instance_camera"
                    lightFs <- fmap catMaybes $ mapM parseLightInstances $ c -=> keep /> tag "instance_light"                
                    geometryFs <- fmap catMaybes $ mapM parseGeometryInstances $ c -=> keep /> tag "instance_geometry"
                    subNodeFs <- fmap catMaybes $ mapM parseNode $ c -=> keep /> (tag "node" `union` tag "instance_node")
                    let treeF refmap = Tree.Node (sid (ColladaNode mid layer transformations (map ($refmap) cameraFs) (map ($refmap) lightFs) (map ($refmap) geometryFs))) (map ($refmap) subNodeFs)
                    case id of (_:_) -> addRefF id $ RefNode . treeF
                    return $ Just treeF
            | otherwise {- "instance_node" -} = do
                    url <- getReqAttribute "url" c
                    let sid = changeTreeSID $ getAttribute "sid" c
                    case localUrl url of
                            Just id -> do assert $ \refmap -> withError (missingLinkErr "instance_node" id c) $ do Just (RefNode _) <- return $ getRef id refmap
                                                                                                                   return ()
                                          return $ Just $ \refmap -> case getRef id refmap of Just (RefNode tree) -> sid tree
                            _ -> return Nothing

parseCameraInstances c = do url <- getReqAttribute "url" c
                            let sid = makeSID $ getAttribute "sid" c
                            case localUrl url of
                                Nothing -> return Nothing
                                Just id -> do assert $ \ refmap -> withError (missingLinkErr "instance_camera" id c) $ do Just (RefCamera _) <- return $ getRef id refmap
                                                                                                                          return ()
                                              return $ Just $ \ refmap -> case getRef id refmap of Just (RefCamera content) -> sid content
parseLightInstances c = do url <- getReqAttribute "url" c
                           let sid = makeSID $ getAttribute "sid" c
                           case localUrl url of
                                Nothing -> return Nothing
                                Just id -> do assert $ \ refmap -> withError (missingLinkErr "instance_light" id c) $ do Just (RefLight _) <- return $ getRef id refmap
                                                                                                                         return ()
                                              return $ Just $ \ refmap -> case getRef id refmap of Just (RefLight content) -> sid content
parseGeometryInstances c = do url <- getReqAttribute "url" c
                              let sid = makeSID $ getAttribute "sid" c
                              case localUrl url of
                                Nothing -> return Nothing
                                Just id -> do assert $ \ refmap -> withError (missingLinkErr "instance_camera" id c) $ do Just (RefGeometry _) <- return $ getRef id refmap
                                                                                                                          return ()
                                              return $ Just $ \ refmap -> case getRef id refmap of Just (RefGeometry content) -> sid content
---------------------------------------------------------------
parseTransformations c = do let sid = makeSID $ getAttribute "sid" c
                            case fst $ head $ tagged keep c of
                                "lookat" -> do xs <- getFromListLengthContents 9 c
                                               let (eye,rest) = splitAt 3 xs
                                                   (int, up) = splitAt 3 rest
                                               return $ Just $ sid $ LookAt (fromListToVec eye) (fromListToVec int) (fromListToVec up)
                                "matrix" -> do mat <- getFromListLengthContents 16 c
                                               return $ Just $ sid $ Matrix $ fromListToMat44 mat
                                "rotate" -> do xs <- getFromListLengthContents 4 c
                                               let (rot,[a]) = splitAt 3 xs
                                               return $ Just $ sid $ Rotate (fromListToVec rot) a
                                "scale" ->  do v <- getFromListLengthContents 3 c
                                               return $ Just $ sid $ Scale $ fromListToVec v
                                "skew" ->   do xs <- getFromListLengthContents 7 c
                                               let ([a],rest) = splitAt 1 xs
                                                   (rot, trans) = splitAt 3 rest
                                               return $ Just $ sid $ Skew a (fromListToVec rot) (fromListToVec trans)
                                "translate" -> do v <- getFromListLengthContents 3 c
                                                  return $ Just $ sid $ Translate $ fromListToVec v
                                _ -> return Nothing
---------------------------------------------------------------

parseVisualColladaTree c = do let id = getAttribute "id" c
                              subNodeFs <- fmap catMaybes $ mapM parseNode $ c -=> keep /> tag "node"
                              unless (null id) $
                                  addRefF id $ \refmap -> RefVisualColladaTree $ Tree.Node (nosid $ ColladaNode (Just id) [] [] [] [] []) $ map ($ refmap) subNodeFs
                                                       
---------------------------------------------------------------

parseCamera c = do let id = getAttribute "id" c
                   optics <- getReqSingleElement "optics" c
                   tech <- getReqSingleElement "technique_common" optics
                   camera <- case (tech -=> keep /> tag "perspective", tech -=> keep /> tag "ortographic") of
                                ([persp], []) -> do fov <- parseViewSize (persp -=> keep /> tag "xfov") (persp -=> keep /> tag "yfov") (persp -=> keep /> tag "aspect_ratio")
                                                                         ("Missing valid combination of xfov, yfov and aspect_ratio elements in " ++ errorPos persp)
                                                    z <- parseZ persp
                                                    return $ Perspective id fov z
                                ([], [orth]) -> do mag <- parseViewSize (orth -=> keep /> tag "xmag") (orth -=> keep /> tag "ymag") (orth -=> keep /> tag "aspect_ratio")
                                                                        ("Missing valid combination of xmag, ymag and aspect_ratio elements in " ++ errorPos orth)
                                                   z <- parseZ orth
                                                   return $ Orthographic id mag z
                                _ -> throwError $ "Excpected one perspective or ortographic element at " ++ errorPos tech
                   addRefF id $ const $ RefCamera camera
    where 
        parseViewSize [x] [] [] _  = do x' <- getFromContents x
                                        return $ ViewSizeX x'
        parseViewSize [] [y] [] _  = do y' <- getFromContents y
                                        return $ ViewSizeY y'
        parseViewSize [x] [y] [] _ = do x' <- getFromContents x
                                        y' <- getFromContents y
                                        return $ ViewSizeXY (V2 x' y')
        parseViewSize [x] [] [a] _ = do x' <- getFromContents x
                                        a' <- getFromContents a
                                        let y' = x' / a'
                                        return $ ViewSizeXY (V2 x' y')
        parseViewSize [] [y] [a] _ = do y' <- getFromContents y
                                        a' <- getFromContents a
                                        let x' = y' * a'
                                        return $ ViewSizeXY (V2 x' y')
        parseViewSize _ _ _ err     = throwError err
        parseZ c = do near <- getReqSingleElement "znear" c
                      znear <- getFromContents near
                      far <- getReqSingleElement "zfar" c
                      zfar <- getFromContents far
                      return $ Z znear zfar
                                              
parseGeometry :: Content Posn
    -> WriterT 
         [(ID, RefMap -> Reference)]
         (WriterT [RefMap ->  Either String ()] (Either String))
         ()
parseGeometry c = do verticess <- mapM (getReqSingleElement "vertices") $ c -=> keep /> cat [tag "convex_mesh", tag "brep"]
                     mapM_ parseVertices verticess
                     mesh <- getReqSingleElement "mesh" c
                     parseColladaMesh (getAttribute "id" c) mesh
                     
parseLight c = do let id = getAttribute "id" c
                  tech <- getReqSingleElement "technique_common" c
                  light <- case (tech -=> keep /> tag "ambient", tech -=> keep /> tag "directional", tech -=> keep /> tag "point", tech -=> keep /> tag "spot") of
                                ([a],[],[],[]) -> do color <- parseSubColor a
                                                     return $ Ambient id color
                                ([],[a],[],[]) -> do color <- parseSubColor a
                                                     return $ Directional id color
                                ([],[],[a],[]) -> do color <- parseSubColor a
                                                     att <- parseSubAttenuation a
                                                     return $ Point id color att
                                ([],[],[],[a]) -> do color <- parseSubColor a
                                                     att <- parseSubAttenuation a
                                                     ang <- getFromSingleElementDef "falloff_angle" 180 c
                                                     exp <- getFromSingleElementDef "falloff_exponent" 0 c
                                                     return $ Spot id color att ang exp
                                _ -> throwError $ "Excpected one ambient, directional, point or spot element at " ++ errorPos tech
                  addRefF id $ const $ RefLight light
    where
        parseSubColor c = do color <- getReqSingleElement "color" c
                             colors <- getFromListLengthContents 3 color
                             return $ fromListToVec colors
        parseSubAttenuation c = do con <- getFromSingleElementDef "constant_attenuation" 1 c
                                   lin <- getFromSingleElementDef "linear_attenuation" 0 c
                                   qua <- getFromSingleElementDef "quadratic_attenuation" 0 c
                                   return $ Attenuation con lin qua

---------------------------------------------------------------
-- ColladaMesh parsing:

parseColladaMesh :: [Char] 
    -> Content Posn 
    -> WriterT
         [(ID, RefMap -> Reference)]
         (WriterT [RefMap -> Either String ()] (Either String))
         ()
parseColladaMesh id c = do v <- getReqSingleElement "vertices" c
                           vertices <- parseVertices v
                           Control.Monad.unless (null id) $
                             do dynPrimListFs <- fmap concat $
                                                   mapM (parsePrimitives vertices) $ children c
                                addRefF id $
                                  \ refmap ->
                                    let dynPrimLists = map (second ($refmap)) dynPrimListFs in
                                      RefGeometry $ ColladaMesh id (makeDynPrimStream dynPrimLists)

          
parseVertices :: Content Posn
    -> WriterT 
         [(ID, RefMap -> Reference)]
         (WriterT [RefMap -> Either String ()] (Either String))
         (RefMap -> Map String ([[Float]], Int))
parseVertices verts = do insF <- fmap (map snd) $ mapM (parseInput False) $ verts -=> keep /> tag "input"
                         let vertsF refmap = Map.unions $ map ($refmap) insF
                         case getAttribute "id" verts of
                            "" -> return ()
                            id -> addRefF id $ RefVertices . vertsF 
                         return vertsF

parseInput :: Bool -> Content Posn -> Parser (Int, RefMap -> Map String ([[Float]], Int))
parseInput shared i = do source <- getReqAttribute "source" i
                         offset <- if shared
                                   then getFromReqAttribute "offset" i
                                   else return undefined
                         case localUrl source of
                            Nothing -> return (offset, const Map.empty)
                            Just id -> do
                                semantic <- getReqAttribute "semantic" i
                                let set = if shared then getAttribute "set" i else ""
                                assert $ \refmap -> case (shared, semantic, getRef id refmap) of
                                                         (true, "VERTEX", Just (RefVertices _)) -> return ()
                                                         (true, "VERTEX", _) -> throwError $ missingLinkErr "vertices" id i
                                                         (_, _, Just (RefSource _)) -> return ()
                                                         _ -> throwError $ missingLinkErr "source" id i
                                let f refmap
                                          = case (shared, semantic, getRef id refmap) of
                                                (true, "VERTEX", Just (RefVertices m)) -> Map.mapKeysMonotonic
                                                                                            (++ set)
                                                                                            m
                                                (_, _, Just (RefSource Nothing)) -> Map.empty
                                                (_, _, Just (RefSource (Just source))) -> Map.singleton
                                                                                            (semantic ++ set)
                                                                                            source
                                return (offset, f)
                                                                       
              
parsePrimitives :: (RefMap -> Map String ([[Float]], Int))
    -> Content Posn
    -> WriterT
         [(ID, RefMap -> Reference)]
         (WriterT [RefMap -> Either String ()] (Either String))
         [((String, PrimitiveTopology Triangles, Maybe [Int]),
           RefMap -> Map String [[Float]])]
parsePrimitives vertices p = case mprimtype of
                       Nothing -> return mempty
                       Just primtype -> do
                               inputFs' <- fmap (Map.fromListWith combine) $ mapM (parseInput True) $ p -=> keep /> tag "input"
                               let inputFs = if Map.null inputFs'
                                                then Map.singleton 0 vertices
                                                else inputFs'
                               count <- getFromReqAttribute "count" p
                               if count == 0
                                    then return []
                                    else do ps' <- case (primtype, ps) of
                                                (TriangleList, _:_:_) -> throwError $ "Multiple p elements in " ++ errorPos p
                                                (TriangleList, [_]) -> return ps
                                                (TriangleList, []) -> throwError $ "Missing p element in " ++ errorPos p
                                                (_, _:_) | length ps < count -> throwError $ "Too few p elements in " ++ errorPos p
                                                _ -> return $ take count ps
                                            mapM (parseP primtype inputFs count) ps'
    where 
      mprimtype :: Maybe (PrimitiveTopology Triangles)
      mprimtype = case fst $ head $ tagged keep p of
        "triangles" -> Just TriangleList
        "trifans" -> Just TriangleFan
        "tristrips" -> Just TriangleStrip
        "polylist" -> Just TriangleList
        _ -> Nothing
      ps :: [Content Posn]
      ps = p -=> keep /> tag "p"
      combine :: (RefMap -> Map String ([[Float]], Int)) -> (RefMap -> Map String ([[Float]], Int)) -> RefMap -> Map String ([[Float]], Int)
      combine f g refmap = f refmap `Map.union` g refmap

parseP :: PrimitiveTopology Triangles -> Map Int (RefMap -> Map String ([[Float]], Int)) -> Int -> Content Posn -> Parser ((String, PrimitiveTopology Triangles, Maybe [Int]), RefMap -> Map String [[Float]])
parseP primtype inputs count p = do let pStride = 1 + fst (Map.findMax inputs)
                                        material = getAttribute "material" p
                                    pLists <- fmap (splitIn pStride) $ do
                                                    pl <- getFromListContents p
                                                    case primtype of
                                                      TriangleList ->
                                                        do when (length pl < count * 3 * pStride) $
                                                             throwError $ "Too few indices in " ++ errorPos p
                                                           return $ take (count * 3 * pStride) pl
                                                      _ -> return pl
                                    case map (first (pLists !!)) $ Map.toList inputs of
                                            [(indices,mF)] -> return ((material, primtype, Just indices), Map.map fst . mF)
                                            xs -> return ((material, primtype, Nothing), combine $ map pickIndices xs)
    where pickIndices (indices, mF) = Map.map pickIndices' . mF
                where pickIndices' (xs, len) = let arr = listArray (0,len) xs in map (arr!) indices
          combine mFs refmap = Map.unions $ map ($refmap) mFs
          splitIn n = reverse . splitIn' [] n -- [[offset 0], [offset 1], [offset 2]]
                where
                    splitIn' acc 0 xs = acc:splitIn' [] n xs
                    splitIn' acc _ [] = []
                    splitIn' acc m (x:xs) = splitIn' (x:acc) (m-1) xs
        

-----------------------------------------------------
-- Dynamic Vertex

makeDynPrimStream :: [((String, PrimitiveTopology Triangles, Maybe [Int]), Map [Char] [[Float]])] -> [ColladaMesh]
makeDynPrimStream = map makePrimGroup . groupBy ((==) `on` fst) . map splitParts

splitParts :: ((t2, t1, t), Map [Char] [[Float]]) -> ((t2, [[Char]], [Int]), (AABB, ((t1, t), [[Float]])))
splitParts ((material, primtype, mindices), m) = let mlist = Map.toAscList m
                                                     ins = map snd mlist
                                                     names = map fst mlist
                                                     sizes = map length $ head ins
                                                     input = map concat ins
                                                     aabb = makeAABB $ Map.lookup "POSITION" m
                                                 in ((material, names, sizes), (aabb,((primtype, mindices), input))) 


makeAABB Nothing = AABB (V3 (-inf) (-inf) (-inf)) (V3 inf inf inf)
makeAABB (Just xs) = mconcat $ map pointToAABB xs
                where pointToAABB (x:y:z:_) = let p = V3 x y z in AABB p p
                      pointToAABB (x:y:_) = AABB (V3 x y (-inf)) (V3 x y inf)
                      pointToAABB (x:_) = AABB (V3 x (-inf) (-inf)) (V3 x inf inf)
                      pointToAABB (_) = AABB (V3 (-inf) (-inf) (-inf)) (V3 inf inf inf)

inf :: Float
inf = read "Infinity"                      
                
makeTypeRep n = dynTypeRep $ makeDyn n undefined

takeBy :: [Int] -> [a] -> [[a]]
takeBy (size:sizes) xs = case splitAt size xs of (a,b) -> a : takeBy sizes b
takeBy [] _ = []

makePrimGroup :: [((String, [Semantic], [Int]), (AABB, ((PrimitiveTopology Triangles, Maybe [Int]), [[Float]])))]
    -> ColladaMesh
makePrimGroup xs@(((material, names, sizes), _):_) = TriangleColladaMesh material desc pstream aabb
    where 
          xs' :: [((String, [Semantic], [Int]), ((PrimitiveTopology Triangles, Maybe [Int]), [[Float]]))]
          xs' = map (second snd) xs
          aabb :: AABB
          aabb = mconcat $ map (fst . snd) xs
          desc :: Map Semantic TypeRep 
          desc = Map.fromAscList $ zip names $ map makeTypeRep sizes
          pstream :: ColladaMeshPrimitiveArray (PrimitiveTopology Triangles) (Map Semantic Dynamic)
          pstream = fmap (Map.fromAscList . zip names . zipWith makeDyn sizes . takeBy sizes) $ makeListStream (sum sizes) xs'

toStreamUsingLength :: [(t, ((p, Maybe [Int]), [[Float]]))] -> ColladaMeshPrimitiveArray p [[Float]]
toStreamUsingLength = fmap (id) . mconcat . map (toPrimStream . second (second (map id)))
-- withLength n v = v `asTypeOf` Vec.mkVec n (undefined :: Vector Float)
toPrimStream :: (t, ((p, Maybe [Int]), a)) -> ColladaMeshPrimitiveArray p a
toPrimStream (_, ((primtype, Just indices), input)) =  ColladaMeshPrimitiveArray $ [ColladaMeshPrimitiveIndexed primtype indices input]
toPrimStream (_, ((primtype, _), input)) = ColladaMeshPrimitiveArray $ [ColladaMeshPrimitive primtype input]

makeDyn n = case n of 
    0 -> const $ toDyn ()
    1 -> toDyn . fromListToVec
    2 -> toDyn . fromListToVec
    3 -> toDyn . fromListToVec
    4 -> toDyn . fromListToVec
    5 -> toDyn . fromListToVec
    6 -> toDyn . fromListToVec
    7 -> toDyn . fromListToVec
    8 -> toDyn . fromListToVec
    9 -> toDyn . fromListToVec
    10 -> toDyn . fromListToVec
    11 -> toDyn . fromListToVec
    12 -> toDyn . fromListToVec
    13 -> toDyn . fromListToVec
    14 -> toDyn . fromListToVec
    15 -> toDyn . fromListToVec
    16 -> toDyn . fromListToVec

makeListStream n = toStreamUsingLength 


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





-- | Render the scene using a simple shading technique through the first camera found, or through a defult camera if the scene doesn't contain any cameras. The scene's lights aren't
--   used in the rendering. The source of this function can also serve as an example of how a Collada 'ColladaTree' can be processed.
-- \root@(sid, node) fs -> 
sceneFromCollada :: ColladaTree -> Scene
sceneFromCollada tree = 
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
            



