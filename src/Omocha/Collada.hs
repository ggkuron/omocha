{-# LANGUAGE RankNTypes
  , TypeFamilies
  , StandaloneDeriving
  , FlexibleContexts 
  , RecordWildCards
  , MultiWayIf
  , GADTs
  , ScopedTypeVariables
  , Arrows
#-}
module Omocha.Collada (
    readColladaFile
    , SID
    , ID
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

import Data.Array (Ix, (!), listArray)
import Data.Map.Strict (Map)
import Data.Function (on)
import Data.Maybe (fromJust, mapMaybe, catMaybes, listToMaybe)
import Data.List hiding (union, transpose)
import qualified Data.Map.Strict as Map
import qualified Data.Foldable as F
import qualified Data.Vector as Vec
import qualified Linear.V as V
import Linear.Matrix hiding (transpose)
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow(initialState)
import Data.Tree.NTree.TypeDefs

import Control.Lens ((^.))
import Control.Monad (join)
import Omocha.Scene(Scene(..), Mesh(..), OmochaShaderType(..), DrawVertex(..), RenderInput(..))
-- import qualified Data.Vector.Unboxed


type ID = String
type SID = Maybe String
type Semantic = String
type MaterialName = String

type ColladaTree = NTree (SID, ColladaNode)
type ColladaColor = V3 Float

data ColladaNode = ColladaNode {
    nodeId:: Maybe ID,
    nodeLayers :: [String],
    nodeTransformations :: [Transform],
    nodeCameras :: [Camera],
    nodeLights :: [Light],
    nodeGeometries :: [Geometry]
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
  } | Directional {
      directionalID :: ID,
      directionalColladaColor :: ColladaColor
  } | Point {
      pointID :: ID,
      pointColladaColor :: ColladaColor,
      pointAttenuation :: Attenuation
  } | Spot {
        spotID :: ID,
        spotColladaColor :: ColladaColor,
        spotAttenuation :: Attenuation,
        spotFallOffAngle :: Float,
        spotFallOffExponent :: Float
  } deriving Show

data Attenuation = Attenuation {
    attenuationConstant:: Float,
    attenuationLinear :: Float,
    attenuationQuandratic :: Float
} deriving (Show, Eq)


newtype Geometry =  Geometry {
    meshPrimitives :: [ColladaMesh]
} deriving (Show)

deriving instance Show Triangles
deriving instance Show a => Show (PrimitiveTopology a)

data ColladaMesh = TriangleColladaMesh {
    meshMaterial :: String,
    meshPrimitiveStream :: ColladaMeshPrimitiveArray (PrimitiveTopology Triangles) (Map Semantic (Vec.Vector (Vec.Vector Float))),
    meshAABB :: AABB
} deriving (Show)


data ColladaMeshPrimitive p a =
    ColladaMeshPrimitive p a
    | ColladaMeshPrimitiveIndexed p (Vec.Vector Int) a
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
        zipAsVector minMax a b =  V.fromV . fromJust . V.fromVector $ Vec.zipWith minMax (V.toVector $ V.toV a) (V.toVector $ V.toV b)



type CArray = (Vec.Vector Float, Int)
type CSource = (Vec.Vector (Vec.Vector Float), Int)
type CVertice = (Vec.Vector (Vec.Vector Float), Int)
type CVertices = Map Semantic CVertice

data Reference = RefNode ColladaTree
    | RefArray CArray
    | RefSource CSource
    | RefVertices CVertices
    | RefVisualColladaTree ColladaTree
    | RefCamera Camera
    | RefLight Light
    | RefGeometry Geometry
    deriving Show

                                                                        
readColladaFile :: FilePath -> IO (Maybe ColladaTree)
readColladaFile f = do
    (_, r) <- runIOSLA (
                        readDocument [ withValidate yes
                                     , withTrace 2
                                     ] f 
                        >>> configSysVars [ withTrace 4]
                        >>> parseDoc 
                        >>> errorMsgStderr
                       )
                       (initialState Map.empty) ()
    print r
    print (length r)
    return $ (listToMaybe . catMaybes) r

localUrl :: ID -> Maybe ID
localUrl ('#':cid) = Just cid
localUrl _ = Nothing

getFromListContents :: forall b. (Read b) => ParseState XmlTree [b]
getFromListContents = getChildren
                      >>> getText 
                      >>> fromList "MalformedContent" 
getFromContents :: forall a. (Read a) => ParseState XmlTree a
getFromContents = getChildren 
                  >>> getText 
                  >>> fromString "MalformedContent" 

getFromListLengthContents :: forall a. (Read a) => Int -> ParseState XmlTree [a]
getFromListLengthContents n = proc c -> do
    xs <- getFromListContents -< c
    if length xs == n
    then returnA -< xs
    else if length xs < n
         then issueErr "TooFewElements" -< xs
         else issueErr "TooManyElements" -< xs

fromList :: forall b. (Read b) => String -> ParseState String [b]
fromList e = listA $ arr words 
                    >>> unlistA 
                    >>> fromString e

fromString :: forall b. (Read b) => String -> ParseState String b
fromString e = proc x -> do
    r <- arr (reads :: ReadS b) -< x
    case r of
        [(a, "")] -> returnA -< a
        _ -> issueErr e -<< read ""

fromListToVec :: forall a. [a] -> V3 a
fromListToVec = V.fromV . fromJust . V.fromVector . Vec.fromList

fromListToMat44 :: forall a. [a] -> M44 a
fromListToMat44 xs = V4 (V4 (xs!!0)  (xs!!1)  (xs!!2)  (xs!!3))
                        (V4 (xs!!4)  (xs!!5)  (xs!!6)  (xs!!7))
                        (V4 (xs!!8)  (xs!!9)  (xs!!10) (xs!!11))
                        (V4 (xs!!12) (xs!!13) (xs!!14) (xs!!15))

traceLog :: Show t => String -> IOSLA (XIOState s) t ()
traceLog msg = proc x -> do
    info <- arr ((" :" ++) . show) -< x
    (issueErr (msg ++ info) >>> unitA) -<< x

singleOnly :: ParseState XmlTree XmlTree
singleOnly = this 

keepWhen :: forall a b cat d. ArrowIf cat => cat a b -> cat b d -> cat a d -> cat a d
keepWhen a b = ifA a (a >>> b)

type ReferenceMap = Map ID Reference
type ParseState a b = IOStateArrow ReferenceMap a b

parseDoc :: ParseState XmlTree (Maybe ColladaTree)
parseDoc = proc xs -> do
    c <- getChildren >>> hasName "COLLADA" -< xs

    listA $ deep $
        isElem 
        >>> catA [ hasName "animation"
                 , hasName "mesh"
                 , hasName "morph"
                 , hasName "skin"
                 , hasName "spline"
                 , hasName "convex_mesh"
                 , hasName "brep"
                 , hasName "nurbs"
                 , hasName "nurbs_surface"
                 ] /> hasName "source"
        >>> parseSource -< c

    listA $ getChildren
        >>> choiceA 
          [
            hasName "library_cameras" :->
                keepWhen (
                    getChildren
                    >>> hasName "camera"
                    >>> hasAttr "id")
                    parseCamera 
                    (issueErr "camera node is not found" >>> unitA),
            hasName "library_geometries" :->
                keepWhen (
                    getChildren
                    >>> hasName "geometry")
                    parseGeometry
                    (issueErr "library_geometries node is not found" >>> unitA),
            hasName "library_lights"  :->
                keepWhen (
                    getChildren
                    >>> hasName "light" 
                    >>> hasAttr "id")
                    parseLight
                    (issueErr "light node is not found" >>> unitA),
            hasName "library_nodes" :->
                keepWhen (
                    getChildren
                    >>> hasName "node")
                    (parseNode >>> unitA)
                    (issueErr "library_node is not found" >>> unitA),
            hasName "library_visual_scenes" :->
                keepWhen (
                    getChildren 
                    >>> hasName "visual_scene")
                    parseVisualColladaTree 
                    (issueErr "library_visual_scenes is not found" >>> unitA)
          ] -< c

    url <- keepWhen
               (getChildren >>> hasName "scene" 
                    >>> (
                        getChildren >>> hasName "instance_visual_scene" 
                                    >>> hasAttr "url"
                    )
                )
               (singleOnly >>> getAttrValue0 "url")
               (issueErr "scene node is not found " >>> (arr . const) "")
               -< c

    state <- getUserState -< ()
    case localUrl url of
      Nothing -> do
         traceLog "url not found" -< url
         returnA -< Nothing
      Just lurl -> do
        case Map.lookup lurl state of
          Just (RefVisualColladaTree t) -> returnA -< Just t
          _ -> issueErr $ "visual_scene element with id '" ++ lurl ++ "' not found when processing" -<< Nothing

---------------------------------------------------------------


parseArray :: ParseState XmlTree ()
parseArray = proc a -> do 
    (name, (cid, (count, ~xs)))
      <- getName 
         &&& getAttrValue0 "id" 
         &&& (getAttrValue0 "count" `withDefault` "-1")
         &&& (getFromListContents >>> arr Vec.fromList)
      -< a
    case name of 
     "float_array" -> do
         let len = Vec.length xs
         if len /= (read count)
         then issueErr "Length of array not the same as the value of count attribute" -< ()
         else do
            changeUserState (\(cid, xs, len) s -> Map.insert cid (RefArray (xs, len)) s) -< (cid, xs, len)
            returnA -< ()
     _ -> issueErr "unknown array" -< ()

---------------------------------------------------------------
          
parseSource :: ParseState XmlTree ()
parseSource = proc x -> do
    listA $
      getChildren 
      >>> hasNameWith (\qn -> "_array" `isSuffixOf` localPart qn)
      >>> parseArray
      -< x
    (cid, source)
     <- getAttrValue "id"
        &&& 
        (
          keepWhen
            (getChildren >>> hasName "technique_common")
            (getChildren >>> hasName "accessor" >>> parseAccessor)
            (arr . const $ Nothing)
        ) -< x

    case source of
      Just s -> do
         changeUserState (\(cid, s) ss -> Map.insert cid (RefSource s) ss) >>> unitA -< (cid, s)
      _ -> returnA -< ()
 
parseAccessor :: ParseState XmlTree (Maybe CSource)
parseAccessor = proc acc -> do
  arrUrl <- getAttrValue "source" -< acc
  case localUrl arrUrl of
       Nothing -> do 
         traceLog "source id is not found: " -< arrUrl
         returnA -< Nothing
       Just cid -> do
         (count, (offset, (stride, useParamList)))
           <- (getAttrValue "count" >>> arr read)
              &&& (getAttrValue0 "offset" `withDefault` "0" >>> arr read) 
              &&& (getAttrValue0 "stride" `withDefault` "1" >>> arr read)
              &&& ((listA $ getChildren 
                   >>> isElem
                   >>> hasName "param" 
                   >>> getAttrValue "name"
                   >>> arr (not . null)) >>> arr Vec.fromList)
              -< acc
         let paramLength = length useParamList
         if paramLength > stride
         then do
             traceLog "StrideAttributeTooLow" -< stride
             returnA -< Nothing
         else do
             let requiredLength = offset + stride * (count-1) + paramLength
             state <- getUserState -< ()
             case Map.lookup cid state of
               Just (RefArray (source, len)) ->
                 if (requiredLength > len) 
                 then issueErr "SourceSizeTooSmall" -< Nothing
                 else do
                     arr
                         (\(source, offset, count, stride, useParamList) ->
                              Just $ (assembleSource 
                                        (Vec.drop offset source)
                                        count 
                                        stride 
                                        useParamList,
                                        count
                                     ))
                       -< (source, offset, count, stride, useParamList)
               _ -> do
                issueErr ("MissingLinkError *_array: " ++ cid) -<< Nothing
  where 
    assembleSource _ 0 _ _ = Vec.empty
    assembleSource source count stride useParamList = case Vec.splitAt stride source of
                                                        (vertex, rest) -> Vec.map snd (Vec.filter fst (Vec.zip useParamList vertex)) `Vec.cons` assembleSource rest (count - 1) stride useParamList

---------------------------------------------------------------

sid :: a -> t -> (Maybe a, t)
sid s a = (Just s, a)
nosid :: t -> (Maybe a, t)
nosid a = (Nothing, a)

makeSID :: forall t. String -> t -> (SID, t)
makeSID "" = nosid
makeSID s = sid s

parseNode :: ParseState XmlTree (Maybe ColladaTree)
parseNode = proc c -> do  
    traceMsg 1 $ "n0:" -< ()
    ifA 
      (hasName "node")
      (proc c -> do
         traceMsg 1 $ "n1:" -< ()
         (cid, (csid, (layer, (transformations, (cameraFs, (lightFs, (geometryFs, subNodeFs)))))))
           <- getAttrValue "id"
              &&&
              (getAttrValue "sid" >>> arr makeSID)
              &&&
              (getAttrValue "layer" >>> arr words)
              &&&
              (listA $ getChildren >>> parseTransformations)
              &&&
              (listA $ getChildren >>> hasName "instance_camera" >>> parseCameraInstances)
              &&&
              (listA $ getChildren >>> hasName "instance_light" >>> parseLightInstances)
              &&&
              (listA $ getChildren >>> hasName "instance_geometry" >>> parseGeometryInstances)
              &&&
              (listA $  (traceMsg 1 "nchild") >>> getChildren >>> (hasName "node" <+> hasName "instance_node") >>> parseNode)
           -< c
         traceMsg 1 $ "n1':" ++ cid -<< ()
         let mid = if cid == "" then Nothing else Just cid
             node = csid $ ColladaNode mid layer (catMaybes transformations) (catMaybes cameraFs) (catMaybes lightFs) (catMaybes geometryFs)
         returnA -< Just $ NTree node $ catMaybes subNodeFs
      )
      (proc c -> do
         traceMsg 1 $ "n2:" -< ()
         url <- getAttrValue "url" -< c
         case localUrl url of
           Just cid -> do
               state <- getUserState -< ()
               case Map.lookup cid state of
                 Just (RefNode t') -> returnA -< Just t'
                 _ -> do
                    issueErr "MissingLinkError instance_node" -< ()
                    returnA -< Nothing
           _ -> do
                issueErr "empty" -< ()
                returnA -< Nothing
      ) -< c

parseCameraInstances :: ParseResult (Maybe Camera)
parseCameraInstances = proc c -> do
    traceMsg 1 $ "cameraI:" -< ()
    url <- getAttrValue "url" -< c
    case localUrl url of
      Nothing -> issueWarn "" -< Nothing
      Just cid -> do
          state <- getUserState -< ()
          case Map.lookup cid state of
               Just (RefCamera camera) -> returnA -< Just camera
               _ -> do
                   issueErr "MissingLinkError instance_camera" -< c
                   returnA -< Nothing

parseLightInstances :: ParseResult (Maybe Light)
parseLightInstances = proc c -> do
    traceMsg 1 $ "lightI:" -< ()
    url <- getAttrValue "url" -< c
    case localUrl url of
      Nothing -> returnA -< Nothing
      Just cid -> do
        state <- getUserState -< ()
        case Map.lookup cid state of
          Just (RefLight l) -> returnA -< Just l
          _ -> do
              issueErr "MissingLinkError instance_light" -< c
              returnA -< Nothing

parseGeometryInstances :: ParseResult (Maybe Geometry)
parseGeometryInstances = proc c -> do
    traceMsg 1 $ "geoI:" -< ()
    url <- getAttrValue "url" -< c
    case localUrl url of
      Nothing -> do
        issueErr ("Missing url: " ++ url) -<< c
        returnA -< Nothing
      Just cid -> do
        traceMsg 1 $ "geoI':" ++ cid -<< ()
        state <- getUserState -< ()
        traceMsg 1 $ "geoI':" ++ (show $ Map.size state) -<< ()
        case Map.lookup cid state of
          Just (RefGeometry g) -> do 
            traceMsg 1 $ "geoI'':" -< ()
            returnA -< Just g
          _ -> do
            issueErr ("MissingLinkError instance geometry: " ++ url) -<< c
            returnA -< Nothing
-------------------------------------------------------------
parseTransformations:: ParseResult (Maybe Transform)
parseTransformations = proc c -> do
    traceMsg 1 $ "trans0:" -< ()
    name <- getName -< c
    case name of
      "lookat" -> do
          xs <- getFromListLengthContents 9 -< c
          let (eye,rest) = splitAt 3 xs
              (int, up) = splitAt 3 rest
          returnA -< Just $ LookAt (fromListToVec eye) (fromListToVec int) (fromListToVec up)
      "matrix" -> do
          mat <- getFromListLengthContents 16 -< c
          returnA -< Just $ Matrix $ fromListToMat44 mat
      "rotate" -> do
          xs <- getFromListLengthContents 4 -< c
          let (rot,[a]) = splitAt 3 xs
          returnA -< Just $ Rotate (fromListToVec rot) a
      "scale" -> do
          v <- getFromListLengthContents 3 -< c
          returnA -< Just $ Scale $ fromListToVec v
      "skew" -> do
          xs <- getFromListLengthContents 7 -< c
          let ([a],rest) = splitAt 1 xs
              (rot, trans) = splitAt 3 rest
          returnA -< Just $ Skew a (fromListToVec rot) (fromListToVec trans)
      "translate" -> do
          v <- getFromListLengthContents 3 -< c
          returnA -< Just $ Translate $ fromListToVec v
      _ -> returnA -< Nothing
---------------------------------------------------------------

parseVisualColladaTree :: ParseState XmlTree ()
parseVisualColladaTree = proc c -> do
    (cid, subNodeFs)
      <- getAttrValue "id"
         &&& (
           listA $ getChildren 
           >>> hasName "node"
           >>> parseNode
         )
       -< c
    if not $ null cid
    then do
      changeUserState (\(cid, subNodeFs) s -> Map.insert cid (RefVisualColladaTree $ NTree (nosid $ ColladaNode (Just cid) [] [] [] [] []) (catMaybes subNodeFs)) s) -< (cid, subNodeFs)
      returnA -< ()
    else do
      issueErr "MissingAttribute id" -< c 
      returnA -< ()
                                                       
---------------------------------------------------------------

unitA :: forall a b. Arrow a => a b ()
unitA = proc _ -> do returnA -< ()

parseCamera :: ParseState XmlTree ()
parseCamera = proc c -> do
    cid <- getAttrValue "id" -< c
    optics <- getChildren >>> hasName "optics" >>> singleOnly -< c 
    tech <- getChildren >>> hasName "technique_common" >>> singleOnly -< optics
    persp <- listA $ getChildren >>> hasName "perspective" -< tech 
    orth <- listA $ getChildren >>> hasName "ortographic" -< tech
    camera 
      <- case (persp, orth) of
           ([p], []) -> do
              xfov <- listA $ getChildren >>> hasName "xfov" -< p
              yfov <- listA $ getChildren >>> hasName "yfov" -< p 
              aspectRatio <- listA $ getChildren >>> hasName "aspect_ratio" -< p
              fov <- parseViewSize "MissingValidPers persp" -< (xfov, yfov, aspectRatio)
              case fov of
                  Just f -> do
                      z <- parseZ -< p
                      returnA -< Just $ Perspective cid f z
                  _ -> returnA -< Nothing
           ([], [orth']) -> do
              xmag <- listA $ getChildren >>> hasName "xmag" -< orth'
              ymag <- listA $ getChildren >>> hasName "ymag" -< orth'
              ratio <- listA $ getChildren >>> hasName "aspect_ratio" -< orth'
              mag <- parseViewSize "MissingValidOrth" -< (xmag, ymag , ratio)
              case mag of
                  Just m -> do
                      z <- parseZ -< orth'
                      returnA -< Just $ Orthographic cid m z
                  _ -> returnA -< Nothing
           _ -> do
              issueErr "MissingView" -< tech
              returnA -< Nothing
    case camera of
        Just camera' -> changeUserState (\_ s -> Map.insert cid (RefCamera camera') s) -<< ()
        _ -> issueErr "not found" -< ()
  where 
    parseViewSize errMsg = proc a -> do
        case a of
          ([x], [], []) -> do
              x' <- getFromContents -< x
              returnA -< Just $ ViewSizeX x'
          ([], [y], []) -> do
              y' <- getFromContents -< y
              returnA -< Just $ ViewSizeY y'
          ([x], [y], []) -> do
              x' <- getFromContents -< x
              y' <- getFromContents -< y
              returnA -< Just $ ViewSizeXY (V2 x' y')
          ([x], [], [a']) -> do
              x' <- getFromContents -< x
              a'' <- getFromContents -< a'
              let y' = x' / a''
              returnA -< Just $ ViewSizeXY (V2 x' y')
          ([], [y], [a']) -> do
              y' <- getFromContents -< y
              a'' <- getFromContents -< a'
              let x' = y' * a''
              returnA -< Just $ ViewSizeXY (V2 x' y')
          _ -> do
              issueErr errMsg -< ()
              returnA -< Nothing
    parseZ = proc p -> do
        near <- getChildren >>> hasName "znear" >>> singleOnly -< p
        znear <- getFromContents -< near
        far <- getChildren >>> hasName "zfar" >>> singleOnly -< p
        zfar <- getFromContents -< far
        returnA -< Z znear zfar
                                              
parseGeometry :: ParseState XmlTree ()
parseGeometry = proc x -> do
   listA $
       keepWhen 
         (getChildren >>> hasName "vertices" >>> singleOnly) 
         (keepWhen 
              (getChildren >>> (hasName "convex_mesh" <+> hasName "brep"))
              (parseVertices >>> unitA)
              (issueErr "vertices root not found v2" >>> unitA))
         (issueErr "vertices not found v1" >>> unitA) -< x
   ifA 
       (getChildren >>> hasName "mesh" >>> singleOnly)
       ((getAttrValue "id"
         &&& (getChildren >>> hasName "mesh" >>> singleOnly))
         >>> parseColladaMesh)
       (issueErr "mesh pair not found" >>> unitA)
       -< x
                     
parseLight :: ParseState XmlTree () -- [(ID, Light)]
parseLight = proc c -> do
    cid <- getAttrValue "id" -< c
    tech <- getChildren >>> hasName "technique_common" >>> singleOnly -< c
    ambient <- listA $ getChildren >>> hasName "ambient" -< tech
    directional <- listA $ getChildren >>> hasName "directional" -< tech
    point <- listA $ getChildren >>> hasName "point" -< tech
    spot <- listA $ getChildren >>> hasName "spot" -< tech
    light
      <- case (ambient, directional, point, spot) of
           ([a],[],[],[]) -> do
             color <- parseSubColor -< a
             returnA -< Just $ Ambient cid color
           ([],[a],[],[]) -> do
             color <- parseSubColor -< a
             returnA -< Just $ Directional cid color
           ([],[],[a],[]) -> do
             color <- parseSubColor -< a
             att <- parseSubAttenuation -< a
             returnA -< Just $ Point cid color att
           ([],[],[],[a]) -> do
             color <- parseSubColor -< a
             att <- parseSubAttenuation -< a
             ang <- getChildren >>> getAttrValue0 "falloff_angle" `withDefault` "180" >>> arr read -< c
             exp' <- getChildren >>> getAttrValue0 "falloff_exponent" `withDefault` "0" >>> arr read -< c
             returnA -< Just $ Spot cid color att ang exp'
           _ -> do
            issueErr "LightError" -< tech
            returnA -< Nothing
    case light of
        Just l -> changeUserState (\_ s -> Map.insert cid (RefLight l) s) -<< ()
        _ -> issueErr "not found" -< ()
  where
    parseSubColor = proc a -> do
        color <- getChildren >>> hasName "color" >>> singleOnly -< a
        colors <- getFromListLengthContents 3 -< color
        returnA -< fromListToVec colors
    parseSubAttenuation = proc a -> do
        con <- getChildren >>> getAttrValue0 "constant_attenuation" `withDefault` "1" >>> arr read -< a
        lin <- getChildren >>> getAttrValue0 "linear_attenuation" `withDefault` "0" >>> arr read -< a
        qua <- getChildren >>> getAttrValue0 "quadratic_attenuation" `withDefault` "0" >>> arr read -< a
        returnA -< Attenuation con lin qua

---------------------------------------------------------------
-- ColladaMesh parsing:

parseColladaMesh :: ParseState (ID, XmlTree) ()
parseColladaMesh = proc (cid, c) -> do
  if not $ null cid 
  then do
    dynPrimListFs
      <- ((getChildren >>> hasName "vertices"
           >>> parseVertices)
          &&& getChildren) >>> parsePrimitives -< c
    (changeUserState 
      (\(cid, dynPrimListFs) s -> Map.insert cid (RefGeometry $ Geometry $ makeDynPrimStream dynPrimListFs) s))
      >>> unitA -< (cid, dynPrimListFs)
  else do
    issueErr ("MissingAttribute " ++ cid) -<< c
    returnA -< ()

type ParseResult a = ParseState XmlTree a

    
parseVertices :: ParseState XmlTree CVertices
parseVertices = proc verts -> do
    (ins, attrId)
     <- (getChildren 
         >>> hasName "input"
         >>> parseInput False)
        &&& getAttrValue "id"
      -< verts
    case ins of
      (Just inf') -> do
          insF <- arr snd -< inf'
          case attrId of
             "" -> do
               issueErr "MissingAttribute" -< verts
               returnA -< insF
             cid -> do
               changeUserState (\(cid, insF) s -> Map.insert cid (RefVertices insF) s) -< (cid, insF)
               returnA -< Map.empty
      _ -> returnA -< Map.empty
    -- let vertsF = Map.unions insF

parseInput :: Bool -> ParseState XmlTree (Maybe (Int, CVertices))
parseInput shared = proc i -> do
    (semantic, (source, (offset, set)))
     <- getAttrValue0 "semantic"
        &&& getAttrValue0 "source"
        &&& getAttrValue "offset"
        &&& getAttrValue "set" -< i

    traceMsg 1 $ ("semantic: " ++ semantic) -<< i
    let offset' = if shared then offset else ""
    case localUrl source of
       Nothing -> do
         issueErr ("Missing source: " ++ source) -<< i
         returnA -< Just (read offset', Map.empty)
       Just cid -> do
         let set' = if shared then set else ""
         state <- getUserState -< ()
         case semantic of
           "VERTEX" ->
             case Map.lookup cid state of
               Just (RefVertices v) -> do
                   traceMsg 1 $ ("v: ") -<< v
                   returnA -< Just (read offset', Map.mapKeysMonotonic (++ set') v)
               _ -> do
                 issueErr ("MissingLinkError vertices: " ++ cid) -<< i
                 returnA -< Nothing
           _ ->
             case Map.lookup cid state of
               Just (RefSource s) -> do
                   traceMsg 1 $ ("s: ") -<< s
                   returnA -< Just $ (read offset', Map.singleton (semantic ++ set') s)
               _ -> do
                   issueErr ("MissingLinkError source: " ++ cid) -<< i
                   returnA -< Nothing
              
parsePrimitives :: ParseState (CVertices, XmlTree) [((MaterialName, PrimitiveTopology Triangles, Maybe (Vec.Vector Int)), Map Semantic (Vec.Vector (Vec.Vector Float)))]
parsePrimitives = proc a@(_, p) -> do
    name <- getName -< p
    case name of
      "triangles" -> parseTriangle TriangleList -< a
      "trifans" -> parseTriangle TriangleFan -< a
      "tristrips" -> parseTriangle TriangleStrip -< a
      "polylist" -> parsePolylist -< a
      _ -> returnA -< mempty
    where 
      combine :: (Map Semantic CVertice) -> (Map Semantic CVertice) ->  Map Semantic CVertice
      combine f g = f `Map.union` g
      combine' mFs = Map.unions $ mFs
      parseTriangle :: PrimitiveTopology Triangles 
        -> ParseState (CVertices, XmlTree) [((MaterialName, PrimitiveTopology Triangles, Maybe (Vec.Vector Int)), Map Semantic (Vec.Vector (Vec.Vector Float)))]
      parseTriangle primtype = proc (vertices, p) -> do
        (inputFs', (count, ~ps'))
          <- (
               listA (getChildren
               >>> hasName "input" 
               >>> parseInput True) >>> arr catMaybes
             )
             &&&
             (
                getAttrValue "count" >>> arr read
             )
             &&&
             (
                listA $ getChildren >>> hasName "p"
             )
             -< p
        let inputFs = if null inputFs'
                      then Map.singleton 0 vertices
                      else Map.fromList inputFs'
        if count == 0
        then returnA -< []
        else do
          case (primtype, ps') of
            (TriangleList, _:_:_) -> do
                issueErr "MultipleElement p" -< p
                returnA -< []
            (TriangleList, [_]) -> unlistA >>> listA (parsePoint primtype inputFs count) -<< ps'
            (TriangleList, []) -> do
                issueErr "MissingElement p" -< p
                returnA -< []
            (_, _:_) | length ps' < count -> do
                issueErr "TooFewElements p" -< p
                returnA -< []
            _ -> do
                ps'' <- arr $ take count -<< ps'
                unlistA >>> listA (parsePoint primtype inputFs count) -<< ps''
         where
           parsePoint :: PrimitiveTopology Triangles 
             -> Map Int CVertices 
             -> Int 
             -> ParseResult ((MaterialName, PrimitiveTopology Triangles, Maybe (Vec.Vector Int)), Map Semantic (Vec.Vector (Vec.Vector Float)))
           parsePoint cprimtype inputs count = proc p' -> do 
               let pStride = 1 + (fst $ Map.findMax inputs) :: Int
               (material, pl)
                <- getAttrValue "material"
                     &&& (getFromListContents >>> arr Vec.fromList) -< p'
               contents <- case cprimtype of
                             TriangleList -> do
                                if (length pl < count * 3 * pStride)
                                then do
                                    issueErr "TooFewIndices" -< p'
                                    returnA -< Vec.empty
                                else do
                                    taken <- arr $ Vec.take (count * 3 * pStride) -<< pl
                                    returnA -< taken
                             _ -> returnA -< pl
               pLists <- arr (splitIn pStride) -<< contents
               case map (first (pLists Vec.!)) $ Map.toList inputs of
                       [(indices, mF)] -> returnA -< ((material, cprimtype, Just indices), Map.map fst mF)
                       xs -> returnA -< ((material, cprimtype, Nothing), combine' $ map pickIndices xs)
      parsePolylist :: ParseState (CVertices, XmlTree) [((MaterialName, PrimitiveTopology Triangles, Maybe (Vec.Vector Int)), Map String (Vec.Vector (Vec.Vector Float)))]
      parsePolylist = proc (vertices, p) -> do
          (inputFs, (count, (ps', vsizes)))
            <- ((listA $ getChildren >>> hasName "input" >>> parseInput True)
               >>> arr (Map.fromListWith combine . catMaybes)
               )
               &&& getAttrValue "count"
               &&& (getChildren >>> hasName "p")
               &&& (getChildren >>> hasName "vcount" >>> getFromListContents >>> arr Vec.fromList)
               -< p 
          let inputFs' = if Map.null inputFs
                         then Map.singleton 0 vertices
                         else inputFs
          if read count == 0
          then returnA -< []
          else listA $ parsePoint -< (inputFs', vsizes, ps')
        where
          parsePoint :: 
            ParseState 
              (Map Int (Map Semantic ((Vec.Vector (Vec.Vector Float)), Int)), Vec.Vector Int, XmlTree)
              ((MaterialName, PrimitiveTopology Triangles, Maybe (Vec.Vector Int)), Map Semantic (Vec.Vector (Vec.Vector Float)))
          parsePoint = proc p'@(inputs, vsizes, ps') -> do 
              let pStride = 1 + fst (Map.findMax inputs)
              (material, pl)
               <- getAttrValue "material"
                  &&& (getFromListContents >>> arr Vec.fromList)
                  -< ps'
              contents 
               <- if length pl < sum vsizes * pStride 
                  then do
                      issueErr "TooFewIndices" -< p'
                      returnA -< Vec.empty
                  else do
                      let taken = Vec.take (sum vsizes * pStride) pl
                      returnA -< taken
              let pLists = splitIn pStride contents
              case map (first (pLists Vec.!)) $ Map.toList inputs of
                      [(indices,mF)] -> returnA -< ((material, TriangleList, Just indices), Map.map fst mF)
                      xs -> returnA -< ((material, TriangleList, Nothing), combine' $ map pickIndices xs)

            
      pickIndices :: (Vec.Vector Int, Map MaterialName (Vec.Vector (Vec.Vector Float), Int)) -> Map Semantic (Vec.Vector (Vec.Vector Float))
      pickIndices (indices, mF) = Map.map (pickIndices' indices) mF
          where
          pickIndices' :: (Ix i, Num i) => Vec.Vector i -> (Vec.Vector b, i) -> Vec.Vector b
          pickIndices' indices' (xs, len) = let a = listArray (0,len) (Vec.toList xs)
                                             in Vec.map (a!) indices'


-----------------------------------------------------
-- Dynamic Vertex

makeDynPrimStream :: [((MaterialName, PrimitiveTopology Triangles, Maybe (Vec.Vector Int)), Map Semantic (Vec.Vector (Vec.Vector Float)))] -> [ColladaMesh]
makeDynPrimStream = catMaybes . map makePrimGroup . groupBy ((==) `on` fst) . map splitParts
  where
    makePrimGroup :: [((MaterialName, [Semantic]), (AABB, ((PrimitiveTopology Triangles, Maybe (Vec.Vector Int)), [Vec.Vector (Vec.Vector Float)])))] -> Maybe ColladaMesh
    makePrimGroup xs@(((material, names), _):_) = Just $ TriangleColladaMesh material pstream aabb
      where 
        xs' :: [((MaterialName, [Semantic]), ((PrimitiveTopology Triangles, Maybe (Vec.Vector Int)), [Vec.Vector (Vec.Vector Float)]))]
        xs' = map (second snd) xs
        aabb :: AABB
        aabb = mconcat $ map (fst . snd) xs
        pstream :: ColladaMeshPrimitiveArray (PrimitiveTopology Triangles) (Map Semantic (Vec.Vector (Vec.Vector Float)))
        pstream = fmap (\f -> Map.fromAscList $ zip names f) $ toStreamUsingLength xs'
          where  
          toStreamUsingLength :: [(t, ((p, Maybe (Vec.Vector Int)), [Vec.Vector (Vec.Vector Float)]))] -> ColladaMeshPrimitiveArray p [Vec.Vector (Vec.Vector Float)]
          toStreamUsingLength = mconcat . map (toPrimStream . second (second id))
            where
            toPrimStream :: (t, ((p, Maybe (Vec.Vector Int)), a)) -> ColladaMeshPrimitiveArray p a
            toPrimStream (_, ((primtype, Just indices), input)) =  ColladaMeshPrimitiveArray [ColladaMeshPrimitiveIndexed primtype indices input]
            toPrimStream (_, ((primtype, _), input)) = ColladaMeshPrimitiveArray [ColladaMeshPrimitive primtype input]
    makePrimGroup _ = Nothing
    splitParts :: ((t2, t1, t), Map Semantic (Vec.Vector (Vec.Vector Float)))
                  -> ((t2, [MaterialName]), (AABB, ((t1, t), [Vec.Vector (Vec.Vector Float)])))
    splitParts ((material, primtype, mindices), m) = let mlist = Map.toAscList m
                                                         ins = map snd mlist
                                                         names = map fst mlist
                                                         input = ins
                                                         aabb = makeAABB $ Map.lookup "POSITION" m
                                                     in ((material, names), (aabb,((primtype, mindices), input))) 


makeAABB :: Maybe (Vec.Vector (Vec.Vector Float)) -> AABB
makeAABB _ = AABB (V3 (-inf) (-inf) (-inf)) (V3 inf inf inf)

-- TODO FIX
-- makeAABB (Just xs) = mconcat $ Vec.map pointToAABB xs
--                 where pointToAABB (x:y:z:_) = let p = V3 x y z in AABB p p
--                       pointToAABB (x:y:_) = AABB (V3 x y (-inf)) (V3 x y inf)
--                       pointToAABB (x:_) = AABB (V3 x (-inf) (-inf)) (V3 x inf inf)
--                       pointToAABB (_) = AABB (V3 (-inf) (-inf) (-inf)) (V3 inf inf inf)

inf :: Float
inf = read "Infinity"                      

-- [[offset 0], [offset 1], [offset 2]]
splitIn :: forall a. Int -> Vec.Vector a -> Vec.Vector (Vec.Vector a) 
splitIn n is = select (Vec.replicate n Vec.empty) $ zip (cycle [0..n - 1]) (Vec.toList is)
    where 
      select acc (x@(m, a):rest) = select (acc Vec.// [(m, acc Vec.! m `Vec.snoc` a)]) rest
      select acc [] = acc

-- | Gets the total transformation matrix of a list of 'Transform' element.
transformsMat :: [Transform] -> M44 Float
transformsMat = foldl (!*!) identity . map transformMat
    where
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
    toRadians :: Floating a => a -> a
    toRadians d = d * pi / 180

-- | The complete transform matrix of all 'Transform' elements in a node.
nodeMat :: ColladaNode -> M44 Float
nodeMat = transformsMat . nodeTransformations

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


aToV3 :: forall a. Vec.Vector a -> V3 a
aToV3 a = V3 (a Vec.! 0) (a Vec.! 2) (a Vec.! 1)

concatMapM :: Monad m => (a -> m (Vec.Vector b)) -> Vec.Vector a -> m (Vec.Vector b)
concatMapM f v = join <$> sequence (fmap f v)

sceneFromCollada :: ColladaTree -> Scene
sceneFromCollada tree = 
    let (_cameras, geometries) = F.foldMap tagContent tree
        primitiveStream = mconcat $ concatMap filterGeometry geometries :: ColladaMeshPrimitiveArray (PrimitiveTopology Triangles) ((Vec.Vector (Vec.Vector Float)), Vec.Vector  (Vec.Vector Float))
    in Scene {
        camera = V3 0 0 0,
        meshes = map toMesh (getColladaMeshPrimitiveArray primitiveStream)
    }
    where
      toMesh :: ColladaMeshPrimitive (PrimitiveTopology Triangles) ((Vec.Vector (Vec.Vector Float)), (Vec.Vector (Vec.Vector Float))) -> Mesh
      toMesh (ColladaMeshPrimitive _ vertices) 
        = Mesh 
            [DrawVertex (aToV3 v) (aToV3 n) (V2 0 0) | (v, n) <-  zip (Vec.toList $ fst vertices) (Vec.toList $ snd vertices)]
            Nothing 
            (V3 0 0 0)
            Nothing
            BoardShader
      toMesh (ColladaMeshPrimitiveIndexed _ indices vertices)
       = Mesh 
          [DrawVertex (aToV3 v) (aToV3 n) (V2 0 0) | (v, n) <- zip (Vec.toList $ fst vertices) (Vec.toList $ snd vertices)]
          (Just $ Vec.toList indices)
          (V3 0 0 0)
          Nothing
          BoardShader
      tagT t = zip (repeat t)
      tagContent (t, n) = (tagT t $ nodeCameras n, tagT t $ nodeGeometries n) 
      filterGeometry (modelMat, (Geometry mesh)) = mapMaybe (filterColladaMesh modelMat) mesh
      filterColladaMesh _modelMat (TriangleColladaMesh _ pstream _aabb) = do
        -- guard $ testAABBprojection modelViewProj aabb /= Outside                -- Frustum cull geometries
        return $ fmap (\v -> let p = fromJust $ Map.lookup "POSITION" v
                                 n = fromJust $ Map.lookup "NORMAL" v
                             in (p, n)
                      ) pstream
            
