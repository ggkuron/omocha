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
module Omocha.Data.AABB (
    AABB(..),
    makeAABB
) where

import Graphics.GPipe (
    V3(..)
    )
import qualified Data.Vector as Vec
import qualified Linear.V as V
import Data.Maybe (fromJust)


data AABB = AABB {
    aabbMin :: V3 Float,
    aabbMax :: V3 Float
} deriving (Show, Eq)

inf :: Float
inf = read "Infinity"                      

instance Monoid AABB where
    mempty = AABB (V3 inf inf inf) (V3 (-inf) (-inf) (-inf))
    mappend (AABB minA maxA) (AABB minB maxB) = AABB (zipAsVector min minA minB) (zipAsVector max maxA maxB)
        where 
        zipAsVector minMax a b =  V.fromV . fromJust . V.fromVector $ Vec.zipWith minMax (V.toVector $ V.toV a) (V.toVector $ V.toV b)

makeAABB :: Maybe (Vec.Vector (Vec.Vector Float)) -> AABB
makeAABB _ = AABB (V3 (-inf) (-inf) (-inf)) (V3 inf inf inf)
