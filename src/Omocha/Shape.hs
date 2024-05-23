module Omocha.Shape where

import Data.Tuple.Extra (both)
import Data.Vector qualified as V
import Foreign (fromBool)
import Graphics.GPipe
import Omocha.MapFile
import Omocha.Mesh
import Omocha.Shader
import RIO
import Prelude (cycle, tail)

plane :: V3 Float -> V3 Float -> V4 Float -> Vector Mesh
plane (V3 w d h) offset color =
  V.singleton
    ( Mesh
        [ Vertex (V3 w d h) (V3 0 1 0) (V2 1 1),
          Vertex (V3 w d 0) (V3 0 1 0) (V2 1 0),
          Vertex (V3 0 d h) (V3 0 1 0) (V2 0 1),
          Vertex (V3 0 d 0) (V3 0 1 0) (V2 0 0)
        ]
        Nothing
        offset
        Nothing
        BoardShader
        (TopologyTriangles TriangleStrip)
        (Just color)
    )

cube :: V3 Float -> V3 Float -> V4 Float -> Vector Mesh
cube (V3 w d h) offset color =
  V.map
    (\v -> Mesh v Nothing offset Nothing BoardShader (TopologyTriangles TriangleStrip) (Just color))
    $ V.fromList
      [ boardByPoints (V3 0 d 0) (V3 0 d h) (V3 w d 0) (V3 w d h),
        boardByPoints (V3 w 0 0) (V3 w 0 h) (V3 0 0 0) (V3 0 0 h),
        boardByPoints (V3 w d h) (V3 w 0 h) (V3 w d 0) (V3 w 0 0),
        boardByPoints (V3 0 d h) (V3 0 d 0) (V3 0 0 h) (V3 0 0 0),
        boardByPoints (V3 0 d h) (V3 0 0 h) (V3 w d h) (V3 w 0 h),
        boardByPoints (V3 w d 0) (V3 w 0 0) (V3 0 d 0) (V3 0 0 0)
      ]

slope :: TipEdge -> (Float, (Float, Float), Float) -> V3 Float -> V4 Float -> Vector Mesh
slope edge (w, (high, low), h) offset color =
  let (a, b, c, d) = case edge of
        RowMin -> (high, low, low, high)
        RowMax -> (low, high, high, low)
        ColumnMin -> (high, high, low, low)
        ColumnMax -> (low, low, high, high)
   in V.map
        (\v -> Mesh v Nothing offset Nothing BoardShader (TopologyTriangles TriangleStrip) (Just color))
        $ V.fromList
          [ boardByPoints (V3 0 a 0) (V3 0 b h) (V3 w d 0) (V3 w c h),
            boardByPoints (V3 w 0 0) (V3 w 0 h) (V3 0 0 0) (V3 0 0 h),
            boardByPoints (V3 w c h) (V3 w 0 h) (V3 w d 0) (V3 w 0 0),
            boardByPoints (V3 0 b h) (V3 0 a 0) (V3 0 0 h) (V3 0 0 0),
            boardByPoints (V3 0 b h) (V3 0 0 h) (V3 w c h) (V3 w 0 h),
            boardByPoints (V3 w d 0) (V3 w 0 0) (V3 0 a 0) (V3 0 0 0)
          ]

cylinder :: Maybe EdgePoint -> V3 Float -> V3 Float -> V4 Float -> Vector Mesh
cylinder center size@(V3 _ depth _) start color =
  let halfSize = size / 2
      (r, offset) =
        case center of
          Nothing -> ((size ^. _xz) / 2, start + V3 (halfSize ^. _x) 0 (halfSize ^. _z))
          Just ep -> (size ^. _xz, start + (\(a, b) -> V3 a 0 b) (both fromBool ep) * size)
      (rstart, rend) = case center of
        Nothing -> (0, (-2) * pi)
        Just (False, False) -> (pi * 0.5, 0)
        Just (False, True) -> (pi * 2, pi * 1.5)
        Just (True, False) -> (pi, pi * 0.5)
        Just (True, True) -> (pi * 1.5, pi)
      topPoints = ellipticalCircle r depth rstart rend
      bottomPoints = map (\p -> p & _y .~ 0) $ reverse topPoints
      facePoints = zip topPoints (tail topPoints)
   in V.map
        ( \p ->
            Mesh
              p
              Nothing
              offset
              Nothing
              BoardShader
              (TopologyTriangles TriangleStrip)
              (Just color)
        )
        ( case center of
            Nothing -> V.empty
            Just (False, False) ->
              V.fromList
                [ boardByNormal (V3 (-1) 0 0) (V2 (size ^. _z) depth) (V3 0 (depth / 2) (halfSize ^. _z)),
                  boardByNormal (V3 0 0 (-1)) (V2 (size ^. _x) depth) (V3 (halfSize ^. _x) (depth / 2) 0)
                ]
            Just (False, True) ->
              V.fromList
                [ boardByNormal (V3 (-1) 0 0) (V2 (size ^. _z) depth) (V3 0 (depth / 2) (-(halfSize ^. _z))),
                  boardByNormal (V3 0 0 1) (V2 (size ^. _x) depth) (V3 (halfSize ^. _x) (depth / 2) 0)
                ]
            Just (True, False) ->
              V.fromList
                [ boardByNormal (V3 1 0 0) (V2 (size ^. _z) depth) (V3 0 (depth / 2) (halfSize ^. _z)),
                  boardByNormal (V3 0 0 (-1)) (V2 (size ^. _x) depth) (V3 (-(halfSize ^. _x)) (depth / 2) 0)
                ]
            Just (True, True) ->
              V.fromList
                [ boardByNormal (V3 1 0 0) (V2 (size ^. _z) depth) (V3 0 (depth / 2) (-(halfSize ^. _z))),
                  boardByNormal (V3 0 0 1) (V2 (size ^. _x) depth) (V3 (-(halfSize ^. _x)) (depth / 2) 0)
                ]
        )
        `V.snoc` Mesh [Vertex p (V3 0 (-1) 0) (V2 0 0) | p <- V3 0 0 0 : bottomPoints] Nothing offset Nothing BoardShader (TopologyTriangles TriangleFan) (Just color)
        `V.snoc` Mesh [Vertex p (V3 0 1 0) (V2 0 0) | p <- V3 0 depth 0 : topPoints] Nothing offset Nothing BoardShader (TopologyTriangles TriangleFan) (Just color)
        `V.snoc` Mesh
          ( do
              (a, c) <- facePoints
              let b = a & _y .~ 0
                  d = c & _y .~ 0
                  center = V3 (r ^. _x) 0 (r ^. _y)
                  n1 = normalize (b - center)
                  n2 = normalize (d - center)
                  uv = V2 0 0
              [ Vertex a n1 uv,
                Vertex b n1 uv,
                Vertex c n2 uv,
                Vertex d n2 uv
                ]
          )
          Nothing
          offset
          Nothing
          BoardShader
          (TopologyTriangles TriangleStrip)
          (Just color)

ellipticalCircle :: (Enum a, Ord a, Floating a, R2 t) => t a -> a -> a -> a -> [V3 a]
ellipticalCircle r y start end =
  [ V3 (r ^. _x * cos t) y (r ^. _y * sin t)
    | t <- interval start end
  ]
  where
    step = 0.125
    interval start end | start > end = [x | x <- [start, start - step .. end], x > end] ++ [end]
    interval start end = [x | x <- [start, start + step .. end], x < end] ++ [end]

cone :: Maybe EdgePoint -> V3 Float -> V3 Float -> V4 Float -> Vector Mesh
cone center size@(V3 _ depth _) offset color =
  let r = (size ^. _xz) / 2
      t = case center of
        Nothing -> V3 (r ^. _x) depth (r ^. _y)
        Just ep -> ((\(a, b) -> V3 a 0 b) (both fromBool ep) * size) & _y .~ depth
      c = size / 2 & _y .~ 0
      bottomPoints = map (+ c) $ ellipticalCircle r 0 (-(2 * pi)) 0
      sides = zip bottomPoints (tail bottomPoints)
   in V.empty
        `V.snoc` Mesh [Vertex p (V3 0 (-1) 0) (V2 0 0) | p <- c : bottomPoints] Nothing offset Nothing BoardShader (TopologyTriangles TriangleFan) (Just color)
        `V.snoc` Mesh
          ( Vertex t (normalize $ t - c) (V2 0 0)
              : ( do
                    (b, d) <- sides
                    let uv = V2 0 0
                    [ Vertex d (normalize $ d - c) uv,
                      Vertex b (normalize $ b - c) uv
                      ]
                )
          )
          Nothing
          offset
          Nothing
          BoardShader
          (TopologyTriangles TriangleFan)
          (Just color)

boardByPoints :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> [Vertex]
boardByPoints a b c d =
  let xs = [a, b, c, d]
      dups = zipWith (\a b -> (a == b, a)) xs (tail $ cycle xs)
      ccw = any fst . take 2 $ dups
      u = map snd . filter (not . fst) $ dups
      n = normalize $ cross (b - a) (c - a)
   in case u of
        [_, _, _, _] ->
          [ Vertex a n (V2 0 1),
            Vertex b n (V2 0 0),
            Vertex c n (V2 1 1),
            Vertex d n (V2 1 0)
          ]
        [a, b, c] | ccw -> triangleByPoints a c b
        [a, b, c] | otherwise -> triangleByPoints a b c
        _ -> []

triangleByPoints :: V3 Float -> V3 Float -> V3 Float -> [Vertex]
triangleByPoints a b c =
  let n = normalize $ cross (b - a) (c - a)
   in [ Vertex a n (V2 0 1),
        Vertex b n (V2 0 0),
        Vertex c n (V2 1 1)
      ]

boardByNormal :: V3 Float -> V2 Float -> V3 Float -> [Vertex]
boardByNormal normal (V2 w h) midPoint =
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
