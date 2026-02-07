{-# LANGUAGE TemplateHaskell #-}

module Astrata.Shape
  ( plane,
    cube,
    divideN,
    rect,
    sphere,
    interval,
    card,
  )
where

import Astrata.Bitmap
import Astrata.MapFile
import Astrata.Mesh
import Data.Vector qualified as V
import Graphics.GPipe hiding (trace)
import Paths_astrata
import RIO hiding (trace, traceStack)

loadBitmapsWith [|getDataFileName|] "../../static/images"

plane :: V3 Float -> V3 Float -> V4 Float -> Vector Mesh
plane (V3 w d h) offset color =
  V.singleton
    ( Mesh
        (boardP' (V3 w d h) (V3 w d 0) (V3 0 d h) (V3 0 d 0))
        Nothing
        offset
        Nothing
        BoardShader
        (TopologyTriangles TriangleStrip)
        (Just color)
    )

card :: V3 Float -> V3 Float -> Either FaceId (V4 Float) -> Vector Mesh
card (V3 w d h) offset t =
  V.imap
    ( \i v ->
        let (tex, c) =
              if i == 0
                then case t of
                  Left Origin0 -> (Just _O0_png, Nothing)
                  Left Origin1 -> (Just _O1_jpg, Nothing)
                  Left Origin2 -> (Just _O2_jpg, Nothing)
                  Left Origin3 -> (Just _O3_jpg, Nothing)
                  Left S1 -> (Just _S1_jpg, Nothing)
                  Left S2 -> (Just _S2_jpg, Nothing)
                  Left S3 -> (Just _S3_jpg, Nothing)
                  Left S4 -> (Just _S4_jpg, Nothing)
                  Left S5 -> (Just _S5_jpg, Nothing)
                  Left S6 -> (Just _S6_jpg, Nothing)
                  Left S7 -> (Just _S7_png, Nothing)
                  Right c -> (Nothing, Just c)
                else (Nothing, Just $ V4 0 1 1 1)
         in Mesh v Nothing offset tex BoardShader (TopologyTriangles TriangleStrip) c
    )
    $ V.fromList
      [ boardP (V3 0 d 0) (V3 0 d h) (V3 w d 0) (V3 w d h), -- top
        boardP (V3 w 0 0) (V3 w 0 h) (V3 0 0 0) (V3 0 0 h), -- bottom
        boardP (V3 w d h) (V3 w 0 h) (V3 w d 0) (V3 w 0 0),
        boardP (V3 0 d h) (V3 0 d 0) (V3 0 0 h) (V3 0 0 0),
        boardP (V3 0 d h) (V3 0 0 h) (V3 w d h) (V3 w 0 h),
        boardP (V3 w d 0) (V3 w 0 0) (V3 0 d 0) (V3 0 0 0)
      ]

cube :: V3 Float -> V3 Float -> V4 Float -> Vector Mesh
cube (V3 w d h) offset color =
  V.map
    (\v -> Mesh v Nothing offset Nothing BoardShader (TopologyTriangles TriangleStrip) (Just color))
    $ V.fromList
      [ boardP (V3 0 d 0) (V3 0 d h) (V3 w d 0) (V3 w d h), -- top
        boardP (V3 w 0 0) (V3 w 0 h) (V3 0 0 0) (V3 0 0 h), -- bottom
        boardP (V3 w d h) (V3 w 0 h) (V3 w d 0) (V3 w 0 0),
        boardP (V3 0 d h) (V3 0 d 0) (V3 0 0 h) (V3 0 0 0),
        boardP (V3 0 d h) (V3 0 0 h) (V3 w d h) (V3 w 0 h),
        boardP (V3 w d 0) (V3 w 0 0) (V3 0 d 0) (V3 0 0 0)
      ]

divideN :: (Fractional a) => a -> a -> Int -> Vector a
divideN from to divs =
  from `V.cons` V.foldl' (\acm i -> acm `V.snoc` (from + step * fromIntegral i)) V.empty (V.generate (divs + 1) (+ 1))
  where
    step = (to - from) / fromIntegral (divs + 1)

interval :: (Ord a, Num a, Enum a, Fractional a) => Int -> a -> a -> Vector a
interval _ start end | start == end = V.singleton start
interval divs start end
  | start > end && step < 0 = start `V.cons` V.unfoldr (gen (>)) (Just 1 :: Maybe Int)
  | start < end && step > 0 = start `V.cons` V.unfoldr (gen (<)) (Just 1 :: Maybe Int)
  where
    step = (end - start) / fromIntegral divs
    gen cmp (Just n) =
      let x = start + fromIntegral n * step
       in Just (if x `cmp` end then (x, Just (n + 1)) else (end, Nothing))
    gen _ Nothing = Nothing
interval _ start end = V.empty `V.snoc` start `V.snoc` end

sphere :: (HasCallStack) => V3 Float -> V3 Float -> V4 Float -> Vector Mesh
sphere size@(V3 w d _) offset color =
  let r = (size ^. _xz) / 2
      c = size / 2 & _y .~ 0
      e1 =
        V.map
          ( \ph ->
              let r' = r ^. _x * sin ph
                  y = d * cos ph
               in V.map
                    ( \th ->
                        let x = r' * cos th
                            z = r' * sin th
                         in V3 x y z
                    )
                    (interval sliceCount 0 (2 * pi))
          )
          (interval stackCount 0 pi)
      r' =
        V.map
          ( \(a :: Vector (V3 Float), b :: Vector (V3 Float)) ->
              V.concatMap
                ( \(a, b) ->
                    let a' = c + a
                        b' = c + b
                     in V.fromList
                          [ Vertex b' (normalize $ b' - c) (V2 0 0),
                            Vertex a' (normalize $ a' - c) (V2 0 0)
                          ]
                )
                (V.zip a b)
          )
          (V.zip e1 (V.tail e1))
   in V.map
        ( \vs ->
            Mesh
              (V.toList vs)
              Nothing
              offset
              Nothing
              BoardShader
              (TopologyTriangles TriangleStrip)
              (Just color)
        )
        r'
  where
    sliceCount = ceiling w * 8
    stackCount = ceiling d * 8

boardP' :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> [Vertex]
boardP' a b c d =
  let n = normalize $ cross (b - a) (c - a)
   in [ Vertex a n (V2 0 0),
        Vertex b n (V2 0 1),
        Vertex c n (V2 1 0),
        Vertex d n (V2 1 1)
      ]

-- 同一頂点を含む場合は向きを保持した三角形を返す
boardP :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> [Vertex]
boardP a b c d =
  let xs = V.fromList [a, b, c, d]
      uniqs = V.zipWith (\a b -> (a == b, a)) xs (V.tail xs `V.snoc` a)
      cw = V.any fst . V.take 2 $ uniqs
      u = V.map snd . V.filter (not . fst) $ uniqs
   in case length u of
        4 -> boardP' a b c d
        3 | cw -> triangleByPoints (u V.! 0) (u V.! 2) (u V.! 1)
        3 | otherwise -> triangleByPoints (u V.! 0) (u V.! 1) (u V.! 2)
        _ -> []

triangleByPoints :: V3 Float -> V3 Float -> V3 Float -> [Vertex]
triangleByPoints a b c =
  let n = normalize $ cross (b - a) (c - a)
   in [ Vertex a n (V2 0 1),
        Vertex b n (V2 0 0),
        Vertex c n (V2 1 1)
      ]

rect :: V3 Float -> V2 Float -> V3 Float -> [Vertex]
rect normal (V2 w h) midPoint =
  let n = normalize normal
      u = if nearZero (n ^. _x) then V3 0 (n ^. _z) (-(n ^. _y)) else V3 (-(n ^. _y)) (n ^. _x) 0
      v = cross n u
      hw = w / 2
      hh = h / 2
      a = midPoint + (hh *^ u) + (hw *^ v)
      b = midPoint - (hh *^ u) + (hw *^ v)
      c = midPoint - (hh *^ u) - (hw *^ v)
      d = midPoint + (hh *^ u) - (hw *^ v)
   in [ Vertex a n (V2 0 1),
        Vertex b n (V2 0 0),
        Vertex d n (V2 1 1),
        Vertex c n (V2 1 0)
      ]
