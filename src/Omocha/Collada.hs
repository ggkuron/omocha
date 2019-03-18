{-# LANGUAGE RankNTypes
  , TypeFamilies
  , StandaloneDeriving
  , FlexibleContexts 
  , RecordWildCards
  , MultiWayIf
  , GADTs
  , ScopedTypeVariables
#-}
module Omocha.Collada (
    readCollada
    , readColladaFile
    , SID
    , ID
    , ColladaTree
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
    , splitIn
) where

import Graphics.GPipe (
    V2(..), V3(..), V4(..),
    dot, outer,
    Triangles,
    PrimitiveTopology(..),
    _x, _y, _z, (^*),
    normalize,
    scaled,
    lookAt,
    axisAngle
    )

import Data.Tree (Tree())
import qualified Data.Tree as Tree
import Data.Array (Ix, (!), listArray)
import Data.Map (Map)
import Data.Function (on)
import Data.Either.Combinators (mapLeft)
import Data.Maybe (fromJust, mapMaybe, catMaybes, maybeToList)
import Data.List hiding (union, transpose)
import qualified Data.Map as Map
import qualified Data.Foldable as F
import qualified Data.Vector as V 
import qualified Linear.V as V
import Linear.Matrix hiding (transpose)
import Text.XML.HaXml (
    (/>),
    tag, keep, tagged, tagWith,
    union,
    elm, attr, attributed,
    with,
    textlabelled,
    deep
    )
import qualified Text.XML.HaXml as XML
import qualified Text.XML.HaXml.Parse as XML
import qualified Text.XML.HaXml.Posn as XML
-- import Text.XML.HXT.Core

import Control.Monad
import Control.Monad.Except
import Control.Arrow (first, second)
import Control.Lens ((^.))
import Omocha.Scene(Scene(..), Mesh(..), OmochaShaderType(..), DrawVertex(..), RenderInput(..))

import Omocha.ColladaTestData


type ID = String
type SID = Maybe String
type Semantic = String
type MaterialName = String


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


data Transform =
    LookAt {
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


data Camera =
    Perspective {
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


data ViewSize =
    ViewSizeX Float
    | ViewSizeY Float
    | ViewSizeXY (V2 Float)
  deriving (Show, Eq)

data Z = Z {
  zNear :: Float,
  zFar :: Float
} deriving (Show, Eq)


data Light =
    Ambient {
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


data Geometry =  Geometry {
    meshPrimitives :: [ColladaMesh]
} deriving (Show)

deriving instance Show Triangles
deriving instance Show a => Show (PrimitiveTopology a)

data ColladaMesh = TriangleColladaMesh {
    meshMaterial :: String,
    meshPrimitiveStream :: ColladaMeshPrimitiveArray (PrimitiveTopology Triangles) (Map Semantic [[Float]]),
    meshAABB :: AABB
} deriving (Show)


data ColladaMeshPrimitive p a =
    ColladaMeshPrimitive p a
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
    mempty = AABB (V3 inf inf inf) (V3 (-inf) (-inf) (-inf))
    mappend (AABB minA maxA) (AABB minB maxB) = AABB (zipAsVector min minA minB) (zipAsVector max maxA maxB)
        where 
        zipAsVector minMax a b =  V.fromV . fromJust . V.fromVector $ V.zipWith minMax (V.toVector $ V.toV a) (V.toVector $ V.toV b)


-- | Traverse a Tree top down, much like 'mapAccumL'. The function recieves an accumulator from its parent and generates one that is passed down to its children.
--   Useful for accumulating transformations, etc.
topDown :: (acc -> x -> (acc, y)) -> acc -> Tree x -> Tree y
topDown f a (Tree.Node x xs) = case f a x of (acc', y) -> Tree.Node y $ map (topDown f acc') xs

-- | Traverse a tree top down accumulating the absolute transform for each node. The projection function enables this to be used on trees that are not 'ColladaTree's.
--   Use 'nodeMat' to get the @Mat44 Float@ from a node. The accumulated matrix for each node will contain the node's own transforms.
topDownTransform :: (x -> M44 Float) -> Tree x -> Tree (M44 Float,x)
topDownTransform f = topDown g identity
    where g t x = let t' = t * f x in (t', (t', x))


toRadians :: Floating a => a -> a
toRadians d = d * pi / 180


type CArray = ([Float], Int)
type CSource = ([[Float]], Int)
type CVertice = ([[Float]], Int)
type CVertices = Map Semantic CVertice


data ColladaParseError where
    MissingLinkError :: Show a => Semantic -> ID -> XML.Content a -> ColladaParseError
    -- DOMError :: Show a => String -> Maybe (XML.Content a) -> ColladaParseError
    NotCollada :: ColladaParseError
    MissingAttribute :: Show a => String -> XML.Content a -> ColladaParseError
    MissingElement :: Show a => String -> XML.Content a -> ColladaParseError
    UnknownElement :: Show a => String -> XML.Content a -> ColladaParseError
    MultipleElement :: Show a => String -> XML.Content a -> ColladaParseError
    MalformedAttribute :: Show a => String -> XML.Content a -> ColladaParseError
    MalformedContent :: Show a => XML.Content a -> ColladaParseError
    TooFewElements :: Show a => XML.Content a -> ColladaParseError
    TooFewIndices :: Show a => XML.Content a -> ColladaParseError
    TooManyElements :: Show a => XML.Content a -> ColladaParseError
    MissingInstanceVisualSceneElement :: ColladaParseError
    MultipleInstanceVisualSceneElement :: ColladaParseError
    ArrayLengthMismatch :: Show a => XML.Content a -> ColladaParseError
    StrideAttributeTooLow :: Show a => XML.Content a -> ColladaParseError
    SourceSizeTooSmall :: Show a => XML.Content a -> ColladaParseError
    MissingValidPers :: Show a => XML.Content a -> ColladaParseError
    MissingValidOrth :: Show a => XML.Content a -> ColladaParseError
    MissingView :: Show a => XML.Content a -> ColladaParseError
    LightError :: Show a => XML.Content a -> ColladaParseError
    UnexpedtedTag :: Show a => XML.Content a -> ColladaParseError
    XmlError :: String -> ColladaParseError


instance Show ColladaParseError where
    show (MissingLinkError semantic cid pos) = semantic ++ " element with id '" ++ cid ++ "' not found when processing " ++ errorPos pos
    show NotCollada = "Expecting COLLADA top-element" 
    show (MissingAttribute s c) = "Missing attribute " ++ s ++ " in " ++ errorPos c
    show (MultipleElement el c) = "Multiple " ++ el ++ " elements in " ++ errorPos c
    show (MissingElement el c) =  "Missing " ++ el ++ " elements in " ++ errorPos c
    show (UnknownElement el c) =  "Unknown element " ++ el ++ " elements in " ++ errorPos c
    show (MalformedAttribute cattr c) = "Malformed " ++ cattr ++ " attribute in " ++ errorPos c
    show (MalformedContent c) = "Malformed contents of " ++ errorPos c
    show (TooFewElements c) = "Too few elements in " ++ errorPos c 
    show (TooFewIndices c) = "Too few indices in " ++ errorPos c 
    show (TooManyElements c) = "Too many elements in " ++ errorPos c
    show MissingInstanceVisualSceneElement = "Missing scene element with instance_visual_scene element found in COLLADA top element."
    show MultipleInstanceVisualSceneElement = "Multiple instance_visual_scene elements in scene element found in COLLADA top element."
    show (ArrayLengthMismatch c) = "Length of array not the same as the value of count attribute in " ++ errorPos c
    show (StrideAttributeTooLow c) = "stride attribute too low in " ++ errorPos c
    show (SourceSizeTooSmall c) = "Source size too small for " ++ errorPos c 
    show (MissingValidPers c) = "Missing valid combination of xfov, yfov and aspect_ratio elements in " ++ errorPos c
    show (MissingValidOrth c) = "Missing valid combination of xmag, ymag and aspect_ratio elements in " ++ errorPos c
    show (MissingView c) = "Excpected one perspective or ortographic element at " ++ errorPos c
    show (LightError c) = "Excpected one ambient, directional, point or spot element at " ++ errorPos c
    show (UnexpedtedTag c) = "Unexpected " ++ errorPos c
    show (XmlError str) = str

errorPos :: Show a => XML.Content a -> String
errorPos (XML.CElem (XML.Elem n _ _) p) = show n ++ " element in " ++ show p ++ "."
errorPos _ = "unknown position"


readCollada :: String -> String -> ParseResult ColladaTree
readCollada f s = do
    p <- mapLeft (return . XmlError) $ XML.xmlParse' f s
    xs <- withError [NotCollada] $ do 
        XML.Document _ _ (XML.Elem (XML.N "COLLADA") _ xs) _ <- return p
        return xs
    parseDoc xs

                                                                        
readColladaFile :: FilePath -> IO ColladaTree
readColladaFile f = do
    s <- readFile f 
    v <- runExceptT $ either throwError return $ readCollada f s
    case v of
        Left e -> throwError . userError $ show e
        Right v' -> return v'


sid :: forall a. String -> a -> (SID, a)
sid s a = (Just s, a)
nosid :: forall a. a -> (SID, a)
nosid a = (Nothing, a)

localUrl :: ID -> Maybe ID
localUrl ('#':cid) = Just cid
localUrl _ = Nothing

withError :: forall a e. e -> Either e a -> Either e a
withError err (Left _) = throwError err
withError _ m = m 

makeSID :: forall t. String -> t -> (SID, t)
makeSID "" = nosid
makeSID s = sid s

changeTreeSID :: forall a. String -> Tree (SID, a) -> Tree (SID, a)
changeTreeSID "" (Tree.Node (_, node) xs) = Tree.Node (nosid node) xs
changeTreeSID s (Tree.Node (_, node) xs) = Tree.Node (sid s node) xs

getAttribute :: forall a. String -> XML.Content a -> String
getAttribute s = fst . head . attributed s keep
getReqAttribute :: String -> XML.Content XML.Posn -> ParseResult String
getReqAttribute s c = case getAttribute s c of
                        "" -> Left $ [MissingAttribute s c]
                        a -> return a

getStringContent :: XML.Content XML.Posn -> String
getStringContent = unwords . mapMaybe fst . textlabelled XML.children

getReqSingleElement :: forall a. (Show a) => String -> XML.Content a -> ParseResult (XML.Content a)
getReqSingleElement el c = case c -=> keep /> tag el of
                            [] -> Left [MissingElement el c]
                            [e] -> return e
                            _ -> Left [MultipleElement el c]

getFromReqAttribute :: forall a. (Read a) => String -> XML.Content XML.Posn -> ParseResult a
getFromReqAttribute cattr c = do
    a <- getReqAttribute cattr c
    fromString [MalformedAttribute cattr c] a

getFromAttributeDef :: forall a. (Read a) =>
    String -> a -> XML.Content XML.Posn -> ParseResult a
getFromAttributeDef cattr def c = do
    let a = getAttribute cattr c
    if null a
        then return def
        else fromString [MalformedAttribute cattr c] a

getFromSingleElementDef :: forall a. (Read a) => String -> a -> XML.Content XML.Posn -> ParseResult a
getFromSingleElementDef el def c = case c -=> keep /> tag el of
                                    [] -> return def
                                    [e] -> getFromContents e
                                    _ -> Left [MultipleElement el c]

getFromListContents :: forall a. (Read a) => XML.Content XML.Posn -> ParseResult [a]
getFromListContents c = fromList [MalformedContent c] $ getStringContent c

getFromContents :: forall a. (Read a) => XML.Content XML.Posn -> ParseResult a
getFromContents c = fromString [MalformedContent c] $ getStringContent c

getFromListLengthContents :: forall a. (Read a ) => Int -> XML.Content XML.Posn -> ParseResult [a]
getFromListLengthContents n c = do
    xs <- getFromListContents c
    if length xs == n
        then return xs
        else if length xs < n 
                then Left [TooFewElements c]
                else Left [TooManyElements c]

fromList :: forall a. (Read a) => [ColladaParseError]  -> String -> ParseResult [a]
fromList err = mapM (fromString err) . words

fromString :: forall a. (Read a) => [ColladaParseError] -> String -> ParseResult a
fromString err = parse . reads
    where parse [(a,"")] = return a
          parse _ = Left err

fromListToVec :: forall a. [a] -> V3 a
fromListToVec = V.fromV . fromJust . V.fromVector . V.fromList

fromListToMat44 :: forall a. [a] -> M44 a
fromListToMat44 xs = V4 (V4 (xs!!0)  (xs!!1)  (xs!!2)  (xs!!3))
                        (V4 (xs!!4)  (xs!!5)  (xs!!6)  (xs!!7))
                        (V4 (xs!!8)  (xs!!9)  (xs!!10) (xs!!11))
                        (V4 (xs!!12) (xs!!13) (xs!!14) (xs!!15))

(==>) :: forall a b t. Foldable t => t a -> (a -> [b]) -> [b]
(-=>) :: t1 -> (t1 -> t) -> t
infixl 2 ==>, -=>
xs ==> f = concatMap f xs
x -=> f = f x


parseDoc :: [XML.Content XML.Posn] -> ParseResult ColladaTree 
parseDoc xs = do
    let sources = xs ==> deep (tagWith (`elem` ["animation", "mesh", "morph", "skin", "spline", "convex_mesh", "brep", "nurbs", "nurbs_surface"])) /> tag "source"
    as <- mapM parseArray $ sources ==> tagged (keep /> tagWith ("_array" `isSuffixOf`) `with` attr "id")
    ss <- mapM (parseSource $ Map.fromList as) sources
    cs <- mapM parseCamera $ xs ==> tag "library_cameras" /> tag "camera" `with` attr "id"
    gs <- mapM (parseGeometry $ Map.fromList ss) $ xs ==> tag "library_geometries" /> tag "geometry"
    ls <- mapM parseLight $ xs ==> tag "library_lights" /> tag "light" `with` attr "id"
    ns <- mapM (parseNode Map.empty (Map.fromList cs) (Map.fromList ls) (Map.fromList gs)) $ xs ==> tag "library_nodes" /> tag "node"
    vs <- mapM (parseVisualColladaTree (Map.fromList ns) (Map.fromList cs) (Map.fromList ls) (Map.fromList gs)) $ xs ==> tag "library_visual_scenes" /> tag "visual_scene"
    (url, c) <- case xs ==> attributed "url" (tag "scene" /> tag "instance_visual_scene") of
          [] -> Left [MissingInstanceVisualSceneElement]
          [x] -> return x
          _ -> Left [MultipleInstanceVisualSceneElement]
    case localUrl url of
           Nothing -> Left [XmlError "url not found"]
           Just lurl -> do
               case Map.lookup lurl (Map.fromList vs) of
                   Just t -> return t
                   _ -> Left [MissingLinkError "visual_scene" lurl c]
---------------------------------------------------------------


parseArray :: (ID, XML.Content XML.Posn) -> ParseResult (ID, CArray)
parseArray (s, arr) = do 
    arrRef <- case s of 
               "float_array" -> do
                   count <- getFromReqAttribute "count" arr
                   xs <- getFromListContents arr
                   let len = length xs
                   if len /= count
                   then Left [ArrayLengthMismatch arr]
                   else return $ (xs :: [Float], len)
               _ -> Left []
    return (getAttribute "id" arr, arrRef)

---------------------------------------------------------------
          
parseSource :: Map ID CArray -> XML.Content XML.Posn -> ParseResult (ID, CSource)
parseSource ma s = do
    cid <- getReqAttribute "id" s
    sRef <- case s -=> keep /> tag "technique_common" of
                 [] -> Left []
                 tc:_ -> do
                    acc <- getReqSingleElement "accessor" tc
                    parseAccessor ma acc
    return (cid, sRef)

parseAccessor :: Map ID CArray -> XML.Content XML.Posn -> ParseResult CSource
parseAccessor ma acc = do
  arrUrl <- getReqAttribute "source" acc
  case localUrl arrUrl of
       Nothing -> Left []
       Just cid -> do
          count <- getFromReqAttribute "count" acc
          offset <- getFromAttributeDef "offset" 0 acc
          stride <- getFromAttributeDef "stride" 1 acc
          useParamList <- mapM parseParam $ acc -=> keep /> elm
          let paramLength = length useParamList
          if paramLength > stride
            then Left [StrideAttributeTooLow acc]
            else do
                let requiredLength = offset + stride * (count-1) + paramLength
                case Map.lookup cid ma of
                  Just (source, len) | requiredLength > len -> Left [SourceSizeTooSmall acc]
                                     | otherwise ->  return (assembleSource (drop offset source) count stride useParamList, count)
                  _ -> Left [MissingLinkError "*_array" cid acc]
  where 
    parseParam c | null $ tag "param" c = Left [UnexpedtedTag c]
                 | otherwise = return $ not $ null $ getAttribute "name" c
    assembleSource _ 0 _ _ = []
    assembleSource source count stride useParamList = case splitAt stride source of (vertex, rest) -> map snd (filter fst (zip useParamList vertex)) : assembleSource rest (count - 1) stride useParamList


---------------------------------------------------------------

parseNode :: 
    Map ID ColladaTree
    -> Map ID Camera
    -> Map ID Light
    -> Map ID Geometry
    -> XML.Content XML.Posn
    -> ParseResult (ID, ColladaTree)
parseNode t mc ml mg c | not $ null $ tag "node" c = do
                               let cid = getAttribute "id" c
                                   mid = if cid == "" then Nothing else Just cid
                                   csid = makeSID $ getAttribute "sid" c
                                   layer = words $ getAttribute "layer" c
                               transformations <- mapM parseTransformations $ XML.children c
                               cameraFs <- mapM (parseCameraInstances mc) $ c -=> keep /> tag "instance_camera"
                               lightFs <- mapM (parseLightInstances ml) $ c -=> keep /> tag "instance_light"                
                               geometryFs <- mapM (parseGeometryInstances mg) $ c -=> keep /> tag "instance_geometry"
                               subNodeFs <- mapM (parseNode t mc ml mg) $ c -=> keep /> (tag "node" `union` tag "instance_node")
                               let treeF = Tree.Node (csid  $ ColladaNode mid layer (catMaybes transformations) cameraFs lightFs geometryFs) (map snd subNodeFs)
                               return (cid, treeF)
                       | otherwise {- "instance_node" -} = do
                               url <- getReqAttribute "url" c
                               let csid = changeTreeSID $ getAttribute "sid" c
                               case localUrl url of
                                       Just cid -> do
                                           case Map.lookup cid t of
                                             Just t' -> return $ (cid, csid t')
                                             _ -> Left [MissingLinkError "instance_node" cid c]
                                       _ -> Left []

parseCameraInstances :: Map ID Camera 
    -> XML.Content XML.Posn 
    -> ParseResult (SID, Camera)
parseCameraInstances mc c = do
    url <- getReqAttribute "url" c
    let csid = makeSID $ getAttribute "sid" c
    case localUrl url of
        Nothing -> Left []
        Just cid -> do
            case Map.lookup cid mc of
                 Just (content) -> return $ csid content
                 _ -> Left [MissingLinkError "instance_camera" cid c]


parseLightInstances :: Map ID Light 
    -> XML.Content XML.Posn 
    -> ParseResult (SID, Light)
parseLightInstances refmap c = do
    url <- getReqAttribute "url" c
    let csid = makeSID $ getAttribute "sid" c
    case localUrl url of
         Nothing -> Left []
         Just cid -> case Map.lookup cid refmap of
                        Just content -> return $ csid content
                        _ -> Left [MissingLinkError "instance_light" cid c]

parseGeometryInstances :: Map ID Geometry 
    -> XML.Content XML.Posn 
    -> ParseResult (SID, Geometry)
parseGeometryInstances refmap c =do
    url <- getReqAttribute "url" c
    let csid = makeSID $ getAttribute "sid" c
    case localUrl url of
      Nothing -> Left []
      Just cid -> do
          case Map.lookup cid refmap of
              Just content -> return $ csid content
              _ -> Left [MissingLinkError "instance_camera" cid c]
---------------------------------------------------------------
parseTransformations:: XML.Content XML.Posn -> ParseResult (Maybe (SID, Transform))
parseTransformations c = do
    let csid = makeSID $ getAttribute "sid" c
    case fst $ head $ tagged keep c of
        "lookat" -> do
            xs <- getFromListLengthContents 9 c
            let (eye,rest) = splitAt 3 xs
                (int, up) = splitAt 3 rest
            return $ Just $ (csid $ LookAt (fromListToVec eye) (fromListToVec int) (fromListToVec up))
        "matrix" -> do
            mat <- getFromListLengthContents 16 c
            return $ Just $ (csid $ Matrix $ fromListToMat44 mat)
        "rotate" -> do
            xs <- getFromListLengthContents 4 c
            let (rot,[a]) = splitAt 3 xs
            return $ Just $ (csid $ Rotate (fromListToVec rot) a)
        "scale" -> do
            v <- getFromListLengthContents 3 c
            return $ Just $ (csid $ Scale $ fromListToVec v)
        "skew" -> do
            xs <- getFromListLengthContents 7 c
            let ([a],rest) = splitAt 1 xs
                (rot, trans) = splitAt 3 rest
            return $ Just $ (csid $ Skew a (fromListToVec rot) (fromListToVec trans))
        "translate" -> do
            v <- getFromListLengthContents 3 c
            return $ Just (csid $ Translate $ fromListToVec v)
        t -> return Nothing
---------------------------------------------------------------

parseVisualColladaTree ::
    Map ID ColladaTree
    -> Map ID Camera
    -> Map ID Light
    -> Map ID Geometry
    -> XML.Content XML.Posn 
    -> ParseResult (ID, ColladaTree)
parseVisualColladaTree mt mc ml mg c = do
    let cid = getAttribute "id" c
    subNodeFs <- mapM (parseNode mt mc ml mg) $ c -=> keep /> tag "node"
    if not $ null cid
       then Right (cid, Tree.Node (nosid $ ColladaNode (Just cid) [] [] [] [] []) (map snd subNodeFs))
       else Left [ MissingAttribute "id" c ]
                                                       
---------------------------------------------------------------

parseCamera :: XML.Content XML.Posn 
    -> ParseResult (ID, Camera)
parseCamera c = do
    let cid = getAttribute "id" c
    optics <- getReqSingleElement "optics" c
    tech <- getReqSingleElement "technique_common" optics
    camera <- case (tech -=> keep /> tag "perspective", tech -=> keep /> tag "ortographic") of
                 ([persp], []) -> do
                    fov <- parseViewSize (persp -=> keep /> tag "xfov") (persp -=> keep /> tag "yfov") (persp -=> keep /> tag "aspect_ratio")
                                         [MissingValidPers persp]
                    z <- parseZ persp
                    return $ Perspective cid fov z
                 ([], [orth]) -> do
                    mag <- parseViewSize (orth -=> keep /> tag "xmag") (orth -=> keep /> tag "ymag") (orth -=> keep /> tag "aspect_ratio")
                                         [MissingValidOrth orth]
                    z <- parseZ orth
                    return $ Orthographic cid mag z
                 _ -> Left [MissingView tech]
    return (cid, camera)
  where 
    parseViewSize [x] [] [] _  = do
        x' <- getFromContents x
        return $ ViewSizeX x'
    parseViewSize [] [y] [] _  = do
        y' <- getFromContents y
        return $ ViewSizeY y'
    parseViewSize [x] [y] [] _ = do
        x' <- getFromContents x
        y' <- getFromContents y
        return $ ViewSizeXY (V2 x' y')
    parseViewSize [x] [] [a] _ = do
        x' <- getFromContents x
        a' <- getFromContents a
        let y' = x' / a'
        return $ ViewSizeXY (V2 x' y')
    parseViewSize [] [y] [a] _ = do
        y' <- getFromContents y
        a' <- getFromContents a
        let x' = y' * a'
        return $ ViewSizeXY (V2 x' y')
    parseViewSize _ _ _ err     = throwError err
    parseZ p = do
        near <- getReqSingleElement "znear" p
        znear <- getFromContents near
        far <- getReqSingleElement "zfar" p
        zfar <- getFromContents far
        return $ Z znear zfar
                                              
parseGeometry ::
   Map ID CSource
   -> XML.Content XML.Posn -> ParseResult (ID, Geometry)
parseGeometry sm c = do
   vertices <- mapM (getReqSingleElement "vertices") $ c -=> keep /> XML.cat [tag "convex_mesh", tag "brep"]
   vm <- mapM (parseVertices sm Map.empty) vertices
   mesh <- getReqSingleElement "mesh" c
   parseColladaMesh sm (Map.fromList vm) (getAttribute "id" c) mesh
                     
parseLight :: XML.Content XML.Posn 
    -> ParseResult (ID, Light)
parseLight c = do
    let cid = getAttribute "id" c
    tech <- getReqSingleElement "technique_common" c
    light <- case (tech -=> keep /> tag "ambient", tech -=> keep /> tag "directional", tech -=> keep /> tag "point", tech -=> keep /> tag "spot") of
                  ([a],[],[],[]) -> do
                    color <- parseSubColor a
                    return $ Ambient cid color
                  ([],[a],[],[]) -> do
                    color <- parseSubColor a
                    return $ Directional cid color
                  ([],[],[a],[]) -> do
                    color <- parseSubColor a
                    att <- parseSubAttenuation a
                    return $ Point cid color att
                  ([],[],[],[a]) -> do
                    color <- parseSubColor a
                    att <- parseSubAttenuation a
                    ang <- getFromSingleElementDef "falloff_angle" 180 c
                    exp' <- getFromSingleElementDef "falloff_exponent" 0 c
                    return $ Spot cid color att ang exp'
                  _ -> Left [LightError tech]
    return (cid, light)
  where
    parseSubColor a = do
        color <- getReqSingleElement "color" a
        colors <- getFromListLengthContents 3 color
        return $ fromListToVec colors
    parseSubAttenuation a = do
        con <- getFromSingleElementDef "constant_attenuation" 1 a
        lin <- getFromSingleElementDef "linear_attenuation" 0 a
        qua <- getFromSingleElementDef "quadratic_attenuation" 0 a
        return $ Attenuation con lin qua

---------------------------------------------------------------
-- ColladaMesh parsing:

parseColladaMesh :: 
    Map ID CSource
    -> Map ID CVertices
    -> ID 
    -> XML.Content XML.Posn 
    -> ParseResult (ID, Geometry)
parseColladaMesh sm vm cid c = do
  v <- getReqSingleElement "vertices" c
  vs <- parseVertices sm vm v
  if not $ null cid 
    then do
      dynPrimListFs <- fmap concat $ mapM (parsePrimitives sm vs) $ XML.children c
      return (cid, Geometry (makeDynPrimStream dynPrimListFs))
    else Left [MissingAttribute cid v]

type ParseResult = Either [ColladaParseError] 

    
parseVertices ::
    Map ID CSource
    -> Map ID CVertices
    -> XML.Content XML.Posn 
    -> ParseResult (ID, CVertices)
parseVertices sm vm verts = do
    insF <- fmap (map snd) $ mapM (parseInput False sm vm) $ verts -=> keep /> tag "input"
    let vertsF = Map.unions insF
    case getAttribute "id" verts of
       "" -> Left [MissingAttribute "id" verts]
       cid -> return (cid, vertsF)

parseInput :: Bool
 -> Map ID CSource
 -> Map ID CVertices
 -> XML.Content XML.Posn 
 -> ParseResult (Int, CVertices)
parseInput shared sm vs i = do
    source <- getReqAttribute "source" i
    ~offset <- if shared
              then getFromReqAttribute "offset" i
              else return undefined
    case localUrl source of
       Nothing -> return (offset, Map.empty)
       Just cid -> do
           semantic <- getReqAttribute "semantic" i
           let set = if shared then getAttribute "set" i else ""
           f <- case semantic of
                     "VERTEX" -> case Map.lookup cid vs of
                                   Just v -> return $ Map.mapKeysMonotonic (++ set) v
                                   _ -> Left [MissingLinkError "vertices" cid i]
                     _ -> case Map.lookup cid sm of
                            Just s -> return $ Map.singleton (semantic ++ set) s
                            _ -> Left [MissingLinkError "source" cid i]
           return (offset, f)
                                                                       
              
parsePrimitives :: Map ID CSource
    -> (ID, CVertices)
    -> XML.Content XML.Posn
    -> ParseResult [((MaterialName, PrimitiveTopology Triangles, Maybe [Int]), Map Semantic [[Float]])]
parsePrimitives sm vs@(_, vertices) p = case fst $ head $ tagged keep p of
                       "triangles" -> parseTriangle TriangleList
                       "trifans" -> parseTriangle TriangleFan
                       "tristrips" -> parseTriangle TriangleStrip
                       "polylist" -> parsePolylist
                       _ -> return mempty
    where 
      ps :: [XML.Content XML.Posn]
      ps = p -=> keep /> tag "p"
      combine :: (Map Semantic CVertice) -> (Map Semantic CVertice) ->  Map Semantic CVertice
      combine f g = f `Map.union` g
      combine' mFs = Map.unions $ mFs
      parseTriangle :: PrimitiveTopology Triangles 
        -> ParseResult [((MaterialName, PrimitiveTopology Triangles, Maybe [Int]), Map Semantic [[Float]])]
      parseTriangle primtype = do
        inputFs' <- mapM (parseInput True sm (Map.fromList [vs])) $ p -=> keep /> tag "input"
        let inputFs = if null inputFs'
                         then Map.singleton 0 vertices
                        else Map.fromList inputFs'
        count <- getFromReqAttribute "count" p
        if count == 0
             then return []
             else do
                ps' <- case (primtype, ps) of
                    (TriangleList, _:_:_) -> Left [MultipleElement "p" p]
                    (TriangleList, [_]) -> return ps
                    (TriangleList, []) -> Left [MissingElement "p" p]
                    (_, _:_) | length ps < count -> Left [TooFewElements p]
                    _ -> return $ take count ps
                mapM (parsePoint primtype inputFs count) ps'
         where
           parsePoint :: PrimitiveTopology Triangles -> Map Int CVertices -> Int -> XML.Content XML.Posn -> ParseResult ((MaterialName, PrimitiveTopology Triangles, Maybe [Int]), Map Semantic [[Float]])
           parsePoint cprimtype inputs count p' = do 
               let pStride = 1 + (fst $ Map.findMax inputs) :: Int
               let material = getAttribute "material" p'
               pLists <- fmap (splitIn pStride) $ do
                               pl <- getFromListContents p'
                               case cprimtype of
                                 TriangleList -> do
                                    when (length pl < count * 3 * pStride) $ Left [TooFewIndices p']
                                    return $ take (count * 3 * pStride) pl
                                 _ -> return pl
               case map (first (pLists !!)) $ Map.toList inputs of
                       [(indices, mF)] -> return ((material, cprimtype, Just indices), Map.map fst mF)
                       xs -> return ((material, cprimtype, Nothing), combine' $ map pickIndices xs)
      parsePolylist :: ParseResult [((MaterialName, PrimitiveTopology Triangles, Maybe [Int]), Map String [[Float]])]
      parsePolylist = do
           inputFs' :: Map Int (Map Semantic ([[Float]], Int))
                <- fmap (Map.fromListWith combine) $ mapM (parseInput True sm (Map.fromList [vs])) $ p -=> keep /> tag "input"
           let inputFs = if Map.null inputFs'
                            then Map.singleton 0 vertices
                            else inputFs'
           count <- getFromReqAttribute "count" p
           if count == 0
                then return []
                else do
                    ps' <-  getReqSingleElement "p"  p
                    vcount <- getReqSingleElement "vcount" p
                    vsizes <- getFromListContents vcount
                    mapM (parsePoint inputFs count vsizes) [ps']
        where
          parsePoint :: Map Int (Map Semantic ([[Float]], Int))
           -> Int 
           -> [Int]
           -> XML.Content XML.Posn 
           -> ParseResult ((MaterialName, PrimitiveTopology Triangles, Maybe [Int]), Map Semantic [[Float]])
          parsePoint inputs _count vsizes p' = do 
              let pStride = 1 + fst (Map.findMax inputs)
                  material = getAttribute "material" p'
              pLists <- fmap (splitIn pStride) $ do
                              pl <- getFromListContents p'
                              when (length pl < sum vsizes * pStride) $
                                Left [TooFewIndices p']
                              return $ take (sum vsizes * pStride) pl
              case map (first (pLists !!)) $ Map.toList inputs of
                      [(indices,mF)] -> return ((material, TriangleList, Just indices), Map.map fst mF)
                      xs -> return ((material, TriangleList, Nothing), combine' $ map pickIndices xs)

            
      pickIndices :: ([Int], Map MaterialName ([[Float]], Int)) -> Map Semantic [[Float]]
      pickIndices (indices, mF) = Map.map (pickIndices' indices) mF
          where
          pickIndices' :: (Ix i, Num i) => [i] -> ([b], i) -> [b]
          pickIndices' indices' (xs, len) = let arr = listArray (0,len) xs in map (arr!) indices'


        

-----------------------------------------------------
-- Dynamic Vertex

makeDynPrimStream :: [((MaterialName, PrimitiveTopology Triangles, Maybe [Int]), Map Semantic [[Float]])] -> [ColladaMesh]
makeDynPrimStream = catMaybes . map makePrimGroup . groupBy ((==) `on` fst) . map splitParts
    where
    makePrimGroup :: [((MaterialName, [Semantic]), (AABB, ((PrimitiveTopology Triangles, Maybe [Int]), [[[Float]]])))] -> Maybe ColladaMesh
    makePrimGroup xs@(((material, names), _):_) = Just $ TriangleColladaMesh material pstream aabb
        where 
              xs' :: [((MaterialName, [Semantic]), ((PrimitiveTopology Triangles, Maybe [Int]), [[[Float]]]))]
              xs' = map (second snd) xs
              aabb :: AABB
              aabb = mconcat $ map (fst . snd) xs
              pstream :: ColladaMeshPrimitiveArray (PrimitiveTopology Triangles) (Map Semantic [[Float]])
              pstream = fmap (\f -> Map.fromAscList $ zip names f) $ toStreamUsingLength xs'
    makePrimGroup _ = Nothing

splitParts :: ((t2, t1, t), Map Semantic [[Float]]) -> ((t2, [MaterialName]), (AABB, ((t1, t), [[[Float]]])))
splitParts ((material, primtype, mindices), m) = let mlist = Map.toAscList m
                                                     ins = map snd mlist
                                                     names = map fst mlist
                                                     input = ins
                                                     aabb = makeAABB $ Map.lookup "POSITION" m
                                                 in ((material, names), (aabb,((primtype, mindices), input))) 


makeAABB :: Maybe [[Float]] -> AABB
makeAABB Nothing = AABB (V3 (-inf) (-inf) (-inf)) (V3 inf inf inf)
makeAABB (Just xs) = mconcat $ map pointToAABB xs
                where pointToAABB (x:y:z:_) = let p = V3 x y z in AABB p p
                      pointToAABB (x:y:_) = AABB (V3 x y (-inf)) (V3 x y inf)
                      pointToAABB (x:_) = AABB (V3 x (-inf) (-inf)) (V3 x inf inf)
                      pointToAABB (_) = AABB (V3 (-inf) (-inf) (-inf)) (V3 inf inf inf)

inf :: Float
inf = read "Infinity"                      
                

-- [[offset 0], [offset 1], [offset 2]]
splitIn :: forall a. Int -> [a] -> [[a]]
splitIn n is = foldl go (replicate n []) $ zip [0..] is
    where 
        go :: [[a]] -> (Int, a) -> [[a]]
        go acc (i, y) = let m = i `mod` n
                         in take m acc ++ [(acc !! m) ++ [y]] ++ drop (m+1) acc



toStreamUsingLength :: [(t, ((p, Maybe [Int]), [[[Float]]]))] -> ColladaMeshPrimitiveArray p [[[Float]]]
toStreamUsingLength = mconcat . map (toPrimStream . second (second (map id)))
toPrimStream :: (t, ((p, Maybe [Int]), a)) -> ColladaMeshPrimitiveArray p a
toPrimStream (_, ((primtype, Just indices), input)) =  ColladaMeshPrimitiveArray $ [ColladaMeshPrimitiveIndexed primtype indices input]
toPrimStream (_, ((primtype, _), input)) = ColladaMeshPrimitiveArray $ [ColladaMeshPrimitive primtype input]



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


aToV3 :: forall a. [a] -> V3 a
aToV3 a = V3 (a!!0) (a!!2) (a!!1)

sceneFromCollada :: ColladaTree -> Scene
sceneFromCollada tree = 
    let (_cameras, geometries) = F.foldMap tagContent $ topDownTransform nodeMat $ fmap snd tree
        primitiveStream = mconcat $ concatMap filterGeometry geometries :: ColladaMeshPrimitiveArray (PrimitiveTopology Triangles) ([[Float]], [[Float]])
    in Scene {
        camera = V3 0 0 0,
        meshes = fmap toMesh (getColladaMeshPrimitiveArray primitiveStream)
    }
    where
      toMesh :: ColladaMeshPrimitive (PrimitiveTopology Triangles) ([[Float]], [[Float]]) -> Mesh
      toMesh (ColladaMeshPrimitive _ vertices) = Mesh [DrawVertex (aToV3 v) (aToV3 n) (V2 0 0) | (v, n) <- zip (fst vertices) (snd vertices) ] Nothing (V3 0 0 0) Nothing BoardShader
      toMesh (ColladaMeshPrimitiveIndexed _ indices vertices) = Mesh [ DrawVertex (aToV3 v) (aToV3 n) (V2 0 0) | (v, n) <- zip (fst vertices) (snd vertices)] (Just indices) (V3 0 0 0) Nothing BoardShader
      tagT t = zip (repeat t)
      tagContent (t, n) = (tagT t $ nodeCameras n, tagT t $ nodeGeometries n) 
      filterGeometry (modelMat, (_,Geometry mesh)) = mapMaybe (filterColladaMesh modelMat) mesh
      filterColladaMesh _modelMat (TriangleColladaMesh _ pstream _aabb) = do
        -- guard $ testAABBprojection modelViewProj aabb /= Outside                -- Frustum cull geometries
        return $ fmap (\v -> let p = fromJust $ Map.lookup "POSITION" v
                                 n = fromJust $ Map.lookup "NORMAL" v
                             in (p, n)
                      ) pstream
            
