module Omocha.MapFile where

import Data.Aeson
import Data.BoundingBox qualified as BB
import Data.Either.Combinators (maybeToRight)
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Vector qualified as V
import Linear.V2
import RIO hiding (map)

data TipEdge
  = RowMin
  | RowMax
  | ColumnMin
  | ColumnMax
  deriving (Show, Generic, Eq)

instance ToJSON TipEdge

instance FromJSON TipEdge

data MapDef
  = Block
      { height :: Float,
        color :: (Float, Float, Float, Float),
        yOffset :: Float
      }
  | Plane
      { color :: (Float, Float, Float, Float),
        yOffset :: Float
      }
  | RTPrism
      { height :: Float,
        color :: (Float, Float, Float, Float),
        yOffset :: Float,
        top :: TipEdge
      }
  deriving (Show, Generic, Eq)

instance ToJSON MapDef

instance FromJSON MapDef

data MapData
  = Tips
      { map :: Vector (Vector Int),
        defs :: M.Map Int MapDef
      }
  | Fill MapDef
  deriving (Show, Generic)

instance FromJSON MapData

instance ToJSON MapData

data MapFile = MapFile
  { size :: (Int, Int),
    mapData :: Vector MapData
  }
  deriving (Show, Generic)

instance FromJSON MapFile

instance ToJSON MapFile

mf :: MapFile
mf =
  MapFile
    { size = (5, 5),
      mapData =
        V.fromList
          [ Tips
              { map =
                  V.fromList
                    . fmap V.fromList
                    $ [ [0, 0, 0, 0, 0],
                        [0, 1, 0, 0, 0],
                        [0, 2, 2, 1, 1],
                        [0, 2, 2, 0, 0],
                        [0, 8, 0, 0, 0]
                      ],
                defs =
                  M.fromList
                    [ (1, Block 1 (0.4, 0.2, 0.4, 1) 0),
                      (2, Block 2 (0.5, 0.5, 0.5, 1) 0),
                      (8, Block 8 (0.5, 0.5, 0.5, 1) 0)
                    ]
              },
            Fill $ Block {height = 0.001, color = (0.2, 0.5, 0.6, 1), yOffset = 0}
          ]
    }

parseMap :: (Int, Int) -> Vector (Vector Int) -> Either String (Vector (BB.Box V2 Int, Int))
parseMap (sw, sh) mapData
  | length mapData /= sh = Left "row mismatch"
  | any (\row -> length row /= sw) mapData = Left "column mismatch"
  | otherwise =
      Right
        $ foldl
          ( \a (V2 x y) ->
              let k = l x y
                  w = ew k x y a
                  h = eh k w x y a
               in (if (k == 0) || (w == 0) then a else (BB.Box (V2 x y) (V2 (x + w) (y + h + 1)), k) `V.cons` a)
          )
          V.empty
          ([V2 x y | y <- [0 .. sh - 1], x <- [0 .. sw - 1]])
  where
    l x y = mapData V.! y V.! x
    ew v x y bbs = length (takeWhile id [v == l x' y && all (\(b, _) -> not $ isInside b x' y) bbs | x' <- [x .. sw - 1]])
    eh v w x y bbs = length (takeWhile id [ew v x y' bbs == w | w > 0, y' <- [y + 1 .. sh - 1]])

isInside :: BB.Box V2 Int -> Int -> Int -> Bool
isInside bb x y = BB.isInside (fmap fromIntegral (V2 x y) + (V2 0.5 0.5 :: V2 Float)) (fmap fromIntegral bb)

parseMapDef :: (Int, Int) -> MapData -> Either String (Vector (BB.Box V2 Int, MapDef))
parseMapDef size (Tips {..}) = do
  bmap <- parseMap size map
  mapM
    ( \(b, k) -> do
        def <- maybeToRight ("key: " ++ show k ++ " not found") (defs M.!? k)
        Right (b, def)
    )
    bmap
parseMapDef (x, y) (Fill def) = Right $ V.singleton (BB.Box (V2 0 0) (V2 x y), def)
