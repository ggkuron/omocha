module Omocha.Shape
  ( plane,
    cube,
    divideN,
    cylinder,
    cylinder',
    slope,
    cone,
    curve,
    curveCross,
    rect,
    tetra,
    sphere,
    interval,
  )
where

import Control.Lens ((+~), (-~))
import Data.Tuple.Extra (both)
import Data.Vector qualified as V
import Foreign (fromBool)
import Graphics.GPipe hiding (trace)
import Omocha.MapFile
import Omocha.Mesh
import Omocha.Spline
import RIO hiding (trace, traceStack)
import Prelude (tail)

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

cube :: V3 Float -> V3 Float -> V4 Float -> Vector Mesh
cube (V3 w d h) offset color =
  V.map
    (\v -> Mesh v Nothing offset Nothing BoardShader (TopologyTriangles TriangleStrip) (Just color))
    $ V.fromList
      [ boardP (V3 0 d 0) (V3 0 d h) (V3 w d 0) (V3 w d h),
        boardP (V3 w 0 0) (V3 w 0 h) (V3 0 0 0) (V3 0 0 h),
        boardP (V3 w d h) (V3 w 0 h) (V3 w d 0) (V3 w 0 0),
        boardP (V3 0 d h) (V3 0 d 0) (V3 0 0 h) (V3 0 0 0),
        boardP (V3 0 d h) (V3 0 0 h) (V3 w d h) (V3 w 0 h),
        boardP (V3 w d 0) (V3 w 0 0) (V3 0 d 0) (V3 0 0 0)
      ]

slope :: Direction -> (Float, (Float, Float), Float) -> V3 Float -> V4 Float -> Vector Mesh
slope edge (w, (high, low), h) offset color =
  let (a, b, c, d) = case edge of
        (Y, False) -> (high, low, high, low)
        (Y, True) -> (low, high, low, high)
        (X, False) -> (high, high, low, low)
        (X, True) -> (low, low, high, high)
   in V.map
        (\v -> Mesh v Nothing offset Nothing BoardShader (TopologyTriangles TriangleStrip) (Just color))
        $ V.fromList
          [ boardP (V3 0 a 0) (V3 0 b h) (V3 w c 0) (V3 w d h),
            boardP (V3 w 0 0) (V3 w 0 h) (V3 0 0 0) (V3 0 0 h),
            boardP (V3 w d h) (V3 w 0 h) (V3 w c 0) (V3 w 0 0),
            boardP (V3 0 a 0) (V3 0 0 0) (V3 0 b h) (V3 0 0 h),
            boardP (V3 0 b h) (V3 0 0 h) (V3 w d h) (V3 w 0 h),
            boardP (V3 w c 0) (V3 w 0 0) (V3 0 a 0) (V3 0 0 0)
          ]

tetra :: (Float, (Float, Float), Float) -> EdgePoint -> V3 Float -> V4 Float -> Vector Mesh
tetra (w, (high, low), h) edge offset color =
  let (a, b, c, d) = case edge of
        (False, False) -> (high, low, low, low)
        (False, True) -> (low, high, low, low)
        (True, False) -> (low, low, high, low)
        (True, True) -> (low, low, low, high)
   in V.map
        (\v -> Mesh v Nothing offset Nothing BoardShader (TopologyTriangles TriangleStrip) (Just color))
        $ V.fromList
          [ boardP (V3 0 a 0) (V3 0 b h) (V3 w c 0) (V3 w d h),
            boardP (V3 w 0 0) (V3 w 0 h) (V3 0 0 0) (V3 0 0 h),
            boardP (V3 w d h) (V3 w 0 h) (V3 w c 0) (V3 w 0 0),
            boardP (V3 0 a 0) (V3 0 0 0) (V3 0 b h) (V3 0 0 h),
            boardP (V3 0 b h) (V3 0 0 h) (V3 w d h) (V3 w 0 h),
            boardP (V3 w c 0) (V3 w 0 0) (V3 0 a 0) (V3 0 0 0)
          ]

divideN :: (Fractional a) => a -> a -> Int -> Vector a
divideN from to divs =
  from `V.cons` V.foldl' (\acm i -> acm `V.snoc` (from + step * fromIntegral i)) V.empty (V.generate (divs + 1) (+ 1))
  where
    step = (to - from) / fromIntegral (divs + 1)

curveCross :: EdgePoint -> V2 Float -> Float -> Float -> Int -> Vector (V2 Float, V2 Float)
curveCross center (V2 w h) width r divs =
  case center of
    (True, True) ->
      let t = divideN (V2 0 h) (V2 w 0) divs
          b = divideN (V2 (min w width) h) (V2 w (min h width)) divs
          t' x = x & _y -~ calcSplines sps (x ^. _x * 2 / w)
          b' x = x & _y -~ calcSplines sps ((x ^. _x - width) * 2 / (w - width))
       in V.map (bimap t' b') (V.zip t b)
    (False, False) ->
      let t = divideN (V2 0 (h - width)) (V2 (w - width) 0) divs
          b = divideN (V2 0 h) (V2 w 0) divs
          t' x = x & _y +~ calcSplines sps ((x ^. _x) * 2 / (w - width))
          b' x = x & _y +~ calcSplines sps (x ^. _x * 2 / w)
       in V.map (bimap t' b') (V.zip t b)
    (True, False) ->
      let t = divideN (V2 width 0) (V2 w (h - width)) divs
          b = divideN (V2 0 0) (V2 w h) divs
          t' x = x & _y +~ calcSplines sps ((x ^. _x - width) * 2 / (w - width))
          b' x = x & _y +~ calcSplines sps (x ^. _x * 2 / w)
       in V.map (bimap t' b') (V.zip t b)
    (False, True) ->
      let t = divideN (V2 0 0) (V2 w h) divs
          b = divideN (V2 0 width) (V2 (w - width) h) divs
          t' x = x & _y -~ calcSplines sps (x ^. _x * 2 / w)
          b' x = x & _y -~ calcSplines sps ((x ^. _x) * 2 / (w - width))
       in V.map (bimap t' b') (V.zip t b)
  where
    sps = spline $ V.fromList [0, r, 0]

curve :: (HasCallStack) => EdgePoint -> V3 Float -> Float -> Float -> V3 Float -> V4 Float -> Vector Mesh
curve center (V3 w d h) width r offset color =
  let cc = curveCross center (V2 w h) width r 16
      tops = V.concatMap (\(t, b) -> Vertex (v3 t d) (V3 0 1 0) (V2 0 0) `V.cons` V.singleton (Vertex (v3 b d) (V3 0 1 0) (V2 0 0))) cc
      bottoms = V.concatMap (\(t, b) -> Vertex (v3 t 0) (V3 0 1 0) (V2 0 0) `V.cons` V.singleton (Vertex (v3 b 0) (V3 0 1 0) (V2 0 0))) (V.reverse cc)
      facePoints =
        ( \ps ->
            let hp = let h = V.take 2 ps in V.zip h (V.tail h)
                lp = let l = V.take 2 (V.reverse ps) in V.zip l (V.tail l)
                sl = V.filter (odd . fst) $ V.imap (,) ps
                sr = V.reverse . V.filter (even . fst) $ V.imap (,) ps
                sl' = V.zipWith (\(_, a) (_, b) -> (a, b)) sl (V.tail sl)
                sr' = V.zipWith (\(_, a) (_, b) -> (a, b)) sr (V.tail sr)
             in hp
                  V.++ sr'
                  V.++ lp
                  V.++ sl'
        )
          tops
   in ( do
          (a, c) <- facePoints
          let b = a.position & _y .~ 0
              d = c.position & _y .~ 0
          return
            $ Mesh
              (boardP a.position b c.position d)
              Nothing
              offset
              Nothing
              BoardShader
              (TopologyTriangles TriangleStrip)
              (Just color)
      )
        `V.snoc` Mesh (V.toList tops) Nothing offset Nothing BoardShader (TopologyTriangles TriangleStrip) (Just color)
        `V.snoc` Mesh (V.toList bottoms) Nothing offset Nothing BoardShader (TopologyTriangles TriangleStrip) (Just color)

cylinder :: Maybe EdgePoint -> V3 Float -> V3 Float -> V4 Float -> Vector Mesh
cylinder = cylinder' 8

cylinder' :: Int -> Maybe EdgePoint -> V3 Float -> V3 Float -> V4 Float -> Vector Mesh
cylinder' divs center size@(V3 _ depth _) (V3 x z y) color =
  let halfSize = size / 2
      (r, offset) = case center of
        Nothing -> ((size ^. _xz) / 2, V3 ((halfSize ^. _x) + x) z ((halfSize ^. _z) + y))
        Just ep -> (size ^. _xz, (\(a, b) -> V3 a 0 b) (both fromBool ep) * size + V3 x z y)
      (rstart, rend) = case center of
        Nothing -> (0, (-2) * pi)
        Just (False, False) -> (pi * 0.5, 0)
        Just (False, True) -> (pi * 2, pi * 1.5)
        Just (True, False) -> (pi, pi * 0.5)
        Just (True, True) -> (pi * 1.5, pi)
      topPoints = ellipticalCircle' r depth (rstart, rend) divs
      bottomPoints = V.toList . V.map (\p -> p & _y .~ 0) $ V.reverse topPoints
      facePoints = V.toList $ V.zip topPoints (V.tail topPoints)
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
                [ rect (V3 (-1) 0 0) (V2 (size ^. _z) depth) (V3 0 (depth / 2) (halfSize ^. _z)),
                  rect (V3 0 0 (-1)) (V2 (size ^. _x) depth) (V3 (halfSize ^. _x) (depth / 2) 0)
                ]
            Just (False, True) ->
              V.fromList
                [ rect (V3 (-1) 0 0) (V2 (size ^. _z) depth) (V3 0 (depth / 2) (-(halfSize ^. _z))),
                  rect (V3 0 0 1) (V2 (size ^. _x) depth) (V3 (halfSize ^. _x) (depth / 2) 0)
                ]
            Just (True, False) ->
              V.fromList
                [ rect (V3 1 0 0) (V2 (size ^. _z) depth) (V3 0 (depth / 2) (halfSize ^. _z)),
                  rect (V3 0 0 (-1)) (V2 (size ^. _x) depth) (V3 (-(halfSize ^. _x)) (depth / 2) 0)
                ]
            Just (True, True) ->
              V.fromList
                [ rect (V3 1 0 0) (V2 (size ^. _z) depth) (V3 0 (depth / 2) (-(halfSize ^. _z))),
                  rect (V3 0 0 1) (V2 (size ^. _x) depth) (V3 (-(halfSize ^. _x)) (depth / 2) 0)
                ]
        )
        `V.snoc` Mesh [Vertex p (V3 0 (-1) 0) (V2 0 0) | p <- V3 0 0 0 : bottomPoints] Nothing offset Nothing BoardShader (TopologyTriangles TriangleFan) (Just color)
        `V.snoc` Mesh [Vertex p (V3 0 1 0) (V2 0 0) | p <- V3 0 depth 0 : V.toList topPoints] Nothing offset Nothing BoardShader (TopologyTriangles TriangleFan) (Just color)
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

ellipticalCircle :: (Enum a, Ord a, Floating a, R2 t) => t a -> a -> (a, a) -> Vector (V3 a)
ellipticalCircle r y range = ellipticalCircle' r y range 8

ellipticalCircle' :: (Enum a, Ord a, Floating a, R2 t) => t a -> a -> (a, a) -> Int -> Vector (V3 a)
ellipticalCircle' r y (start, end) divs =
  V.map (\t -> V3 (r ^. _x * cos t) y (r ^. _y * sin t)) $ interval divs start end

cone :: Maybe EdgePoint -> V3 Float -> V3 Float -> V4 Float -> Vector Mesh
cone center size@(V3 _ depth _) offset color =
  let r = (size ^. _xz) / 2
      t = case center of
        Nothing -> V3 (r ^. _x) depth (r ^. _y)
        Just ep -> ((\(a, b) -> V3 a 0 b) (both fromBool ep) * size) & _y .~ depth
      c = size / 2 & _y .~ 0
      bottomPoints = V.toList $ V.map (+ c) $ ellipticalCircle r 0 (-(2 * pi), 0)
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
   in [ Vertex a n (V2 0 1),
        Vertex b n (V2 0 0),
        Vertex c n (V2 1 1),
        Vertex d n (V2 1 0)
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

v3 :: (R2 t) => t a -> a -> V3 a
v3 v2 z = V3 (v2 ^. _x) z (v2 ^. _y)
