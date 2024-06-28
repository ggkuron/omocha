module Omocha.Shape
  ( plane,
    cube,
    cylinder,
    slope,
    cone,
    rect,
    sphere,
    interval,
    splined3,
    boardXZ,
    boardZ,
  )
where

import Control.Lens ((-~))
import Data.Tuple.Extra (both)
import Data.Vector qualified as V
import Foreign (fromBool)
import Graphics.GPipe hiding (trace)
import Omocha.MapFile
import Omocha.Mesh
import Omocha.Spline
import RIO hiding (trace, traceStack)
import Prelude (tail)

type MapSplines = SplinePairs Float

plane :: (HasCallStack) => Point -> V3 Float -> Float -> V3 Float -> V4 Float -> MapSplines -> V2 Int -> Vector Mesh
plane p unit d offset color sps divs =
  let vs = boardXZ unit sps p divs d d d d
   in V.map
        ( \v ->
            Mesh
              (V.toList v)
              Nothing
              offset
              Nothing
              BoardShader
              (TopologyTriangles TriangleStrip)
              (Just color)
        )
        vs

type Point = ((Float, Float), (Float, Float))

-- heightが一定
cube :: (HasCallStack) => Point -> V3 Float -> Float -> V3 Float -> V4 Float -> MapSplines -> V2 Int -> Vector Mesh
cube p unit d = cube' p unit d d d d

slope :: (HasCallStack) => Point -> V3 Float -> Direction -> (Float, Float) -> V3 Float -> V4 Float -> MapSplines -> V2 Int -> Vector Mesh
slope p unit edge (high, low) offset color sps divs =
  let (a, b, c, d) = case edge of
        (Y, False) -> (high, low, high, low)
        (Y, True) -> (low, high, low, high)
        (X, False) -> (high, high, low, low)
        (X, True) -> (low, low, high, high)
   in cube' p unit a b c d offset color sps divs

-- heightが一定でない
cube' :: (HasCallStack) => Point -> V3 Float -> Float -> Float -> Float -> Float -> V3 Float -> V4 Float -> MapSplines -> V2 Int -> Vector Mesh
cube' ((x, w), (y, h)) unit az bz cz dz offset color sps divs =
  let top = boardXZ unit sps ((x, w), (y, h)) divs az bz cz dz
      bottom = boardXZ unit sps ((x, w), (h, y)) divs 0 0 0 0
      facePoints =
        V.concatMap
          ( \ls ->
              let ps = V.map (.position) ls
                  hp = let h = V.take 2 ps in V.zip h (V.tail h)
                  lp = let l = V.take 2 (V.reverse ps) in V.zip l (V.tail l)
                  sl = V.filter (odd . fst) $ V.imap (,) ps
                  sr = V.reverse . V.filter (even . fst) $ V.imap (,) ps
                  sl' = V.zipWith (\(_, a) (_, b) -> (a, b)) sl (V.tail sl)
                  sr' = V.zipWith (\(_, a) (_, b) -> (a, b)) sr (V.tail sr)
               in hp V.++ lp V.++ sl' V.++ sr'
          )
          top
   in ( do
          (a, c) <- facePoints
          let b = a & _y .~ 0
              d = c & _y .~ 0
          return
            $ Mesh
              (boardP 1 a b c d)
              Nothing
              offset
              Nothing
              BoardShader
              (TopologyTriangles TriangleStrip)
              (Just color)
      )
        V.++ V.map
          (\p -> Mesh (V.toList p) Nothing offset Nothing BoardShader (TopologyTriangles TriangleStrip) (Just color))
          (top V.++ bottom)

cylinder :: (HasCallStack) => Point -> V3 Float -> Maybe EdgePoint -> Float -> V3 Float -> V4 Float -> MapSplines -> V2 Int -> Vector Mesh
cylinder ((x, w), (y, h)) unit center d offset color sps divs =
  let halfSize = size / 2
      (w', h') = (w - x, h - y)
      size = V3 w' d h'
      depth = d
      (r, offset') = case center of
        Nothing -> ((size ^. _xz) / 2, V3 ((halfSize ^. _x) + x) 0 ((halfSize ^. _z) + y))
        Just ep -> (size ^. _xz, (\(a, b) -> V3 a 0 b) (both fromBool ep) * size + V3 x 0 y)
      (rstart, rend) = case center of
        Nothing -> (0, (-2) * pi)
        Just (False, False) -> (pi * 0.5, 0)
        Just (False, True) -> (pi * 2, pi * 1.5)
        Just (True, False) -> (pi, pi * 0.5)
        Just (True, True) -> (pi * 1.5, pi)
      topPoints = ellipticalCircle r depth (rstart, rend)
      bottomPoints = V.toList . V.map (\p -> p & _y .~ 0) $ V.reverse topPoints
      facePoints = V.toList $ V.zip topPoints (V.tail topPoints)
   in V.map
        ( \p ->
            Mesh
              (V.toList p)
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
              V.empty
                `V.snoc` boardZ unit sps (V2 x y, V2 w y) divs depth depth depth
                `V.snoc` boardZ unit sps (V2 x y, V2 x h) divs depth depth depth
            Just (False, True) ->
              V.empty
                `V.snoc` boardZ unit sps (V2 x h, V2 w h) divs depth depth depth
                `V.snoc` boardZ unit sps (V2 x y, V2 x h) divs depth depth depth
            Just (True, False) ->
              V.empty
                `V.snoc` boardZ unit sps (V2 x y, V2 w y) divs depth depth depth
                `V.snoc` boardZ unit sps (V2 w y, V2 w h) divs depth depth depth
            Just (True, True) ->
              V.empty
                `V.snoc` boardZ unit sps (V2 x h, V2 w h) divs depth depth depth
                `V.snoc` boardZ unit sps (V2 w h, V2 w y) divs depth depth depth
        )
        `V.snoc` Mesh [Vertex (unit * splined3 sps (p + offset')) (V3 0 (-1) 0) (V2 0 0) | p <- V3 0 0 0 : bottomPoints] Nothing offset Nothing BoardShader (TopologyTriangles TriangleFan) (Just color)
        `V.snoc` Mesh [Vertex (unit * splined3 sps (p + offset')) (V3 0 1 0) (V2 0 0) | p <- V3 0 depth 0 : V.toList topPoints] Nothing offset Nothing BoardShader (TopologyTriangles TriangleFan) (Just color)
        `V.snoc` Mesh
          ( do
              (a, c) <- facePoints
              let b = a & _y .~ 0
                  d = c & _y .~ 0
                  center = V3 (r ^. _x) 0 (r ^. _y)
                  n1 = normalize (b - center)
                  n2 = normalize (d - center)
                  uv = V2 0 0
              [ Vertex (unit * splined3 sps (a + offset')) n1 uv,
                Vertex (unit * splined3 sps (b + offset')) n1 uv,
                Vertex (unit * splined3 sps (c + offset')) n2 uv,
                Vertex (unit * splined3 sps (d + offset')) n2 uv
                ]
          )
          Nothing
          offset
          Nothing
          BoardShader
          (TopologyTriangles TriangleStrip)
          (Just color)

repeatUntilLengthsMatch :: Int -> Vector a -> Vector a
repeatUntilLengthsMatch _ as | null as = V.empty
repeatUntilLengthsMatch n as =
  let l = length as
   in as V.++ V.fromList (replicate (n - l) (V.last as))

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

cone :: (HasCallStack) => Point -> V3 Float -> Maybe EdgePoint -> Float -> V3 Float -> V4 Float -> MapSplines -> Vector Mesh
cone ((x, w), (y, h)) unit center depth offset color sps =
  let (w', h') = (w - x, h - y)
      size = V3 w' depth h'
      r = (size ^. _xz) / 2
      t' =
        unit
          * splined3
            sps
            ( V3 x 0 y
                + case center of
                  Nothing -> v3 r depth
                  Just ep -> ((\(a, b) -> V3 a 0 b) (both fromBool ep) * size) & _y .~ depth
            )
      c = V3 x 0 y + size / 2 & _y .~ 0
      c' = unit * splined3 sps c
      bottomPoints = V.toList . V.map (+ c) $ ellipticalCircle r 0 (-(2 * pi), 0)
      sides = zip bottomPoints (tail bottomPoints)
   in V.empty
        `V.snoc` Mesh [Vertex (unit * splined3 sps p) (V3 0 (-1) 0) (V2 0 0) | p <- c : bottomPoints] Nothing offset Nothing BoardShader (TopologyTriangles TriangleFan) (Just color)
        `V.snoc` Mesh
          ( Vertex t' (normalize $ t' - c') (V2 0 0)
              : ( do
                    s <- sides
                    let uv = V2 0 0
                        (b', d') = both (\t -> unit * splined3 sps t) s
                    [ Vertex d' (normalize $ d' - c') uv,
                      Vertex b' (normalize $ b' - c') uv
                      ]
                )
          )
          Nothing
          offset
          Nothing
          BoardShader
          (TopologyTriangles TriangleFan)
          (Just color)

sphere :: (HasCallStack) => Point -> V3 Float -> Float -> V3 Float -> V4 Float -> MapSplines -> Vector Mesh
sphere ((x, w), (y, h)) unit d offset color sps =
  let size = V3 (w - x) d (h - y)
      r = (size ^. _xz) / 2
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
      c' = unit * splined3 sps (c + V3 x 0 y)
      r' =
        V.map
          ( \(a :: Vector (V3 Float), b :: Vector (V3 Float)) ->
              V.concatMap
                ( \(a, b) ->
                    let a' = c' + unit * a
                        b' = c' + unit * b
                     in V.fromList
                          [ Vertex b' (normalize $ b' - c') (V2 0 0),
                            Vertex a' (normalize $ a' - c') (V2 0 0)
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

boardP' :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> V3 Float -> [Vertex]
boardP' unit a b c d =
  let n = normalize $ cross (b - a) (c - a)
   in [ Vertex (a * unit) n (V2 0 1),
        Vertex (b * unit) n (V2 0 0),
        Vertex (c * unit) n (V2 1 1),
        Vertex (d * unit) n (V2 1 0)
      ]

-- 同一頂点を含む場合は向きを保持した三角形を返す
boardP :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> V3 Float -> [Vertex]
boardP unit a b c d =
  let xs = V.fromList [a, b, c, d]
      uniqs = V.zipWith (\a b -> (a == b, a)) xs (V.tail xs `V.snoc` a)
      cw = V.any fst . V.take 2 $ uniqs
      u = V.map snd . V.filter (not . fst) $ uniqs
   in case length u of
        4 -> boardP' unit a b c d
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

boardXZ :: (HasCallStack) => V3 Float -> MapSplines -> ((Float, Float), (Float, Float)) -> V2 Int -> Float -> Float -> Float -> Float -> Vector (Vector Vertex)
boardXZ unit sps range@(rangeX, rangeY) (V2 divX divY) az bz cz dz =
  let (isLinearX, isLinearY) = both (V.all (uncurry (&&) . both isLinear)) sps
      sizeY = abs $ snd rangeY - fst rangeY
      divY = 4
   in if
        | isLinearX && isLinearY ->
            let a = splined sps (V2 (fst rangeX) (fst rangeY))
                b = splined sps (V2 (fst rangeX) (snd rangeY))
                c = splined sps (V2 (snd rangeX) (fst rangeY))
                d = splined sps (V2 (snd rangeX) (snd rangeY))
             in V.singleton $ V.fromList $ boardP' unit (v3 a az) (v3 b bz) (v3 c cz) (v3 d dz)
        | isLinearX -> V.singleton $ divideAlong X unit sps range az bz cz dz
        | isLinearY -> V.singleton $ divideAlong Y unit sps range az cz bz dz
        | otherwise ->
            V.generate
              divY
              ( \i ->
                  let zAB n = az + ((bz - az) / (fromIntegral divY - 1)) * fromIntegral n
                      zCD n = cz + ((dz - cz) / (fromIntegral divY - 1)) * fromIntegral n
                      yRange = both (\i -> fromIntegral i * sizeY / (fromIntegral divY - 1)) (i, i + 1)
                   in divideAlong X unit sps (rangeX, yRange) (zAB i) (zAB (i + 1)) (zCD i) (zCD (i + 1))
              )
  where
    divideAlong :: (HasCallStack) => Axis -> V3 Float -> MapSplines -> ((Float, Float), (Float, Float)) -> Float -> Float -> Float -> Float -> Vector Vertex
    divideAlong _ _ sps _ _ _ _ _ | null (fst sps) = V.empty
    divideAlong _ _ sps _ _ _ _ _ | null (snd sps) = V.empty
    divideAlong axis unit sps (range_1, range_2) az bz cz dz =
      let v2 = case axis of
            X -> flip V2
            Y -> V2
          (crossSeg, alongSeg) = case axis of
            X -> (range_2, range_1)
            Y -> (range_1, range_2)
          (_alongDiv, crossDiv) = case axis of
            X -> (divY, divX)
            Y -> (divX, divY)
          crossPoints = uncurry (interval crossDiv) crossSeg

          ps = V.map (\cp -> both (v2 cp) alongSeg) crossPoints
       in if length ps < 2
            then V.empty
            else
              let zAB n = az + ((bz - az) / fromIntegral (length ps - 1)) * fromIntegral n
                  zCD n = cz + ((dz - cz) / fromIntegral (length ps - 1)) * fromIntegral n
                  v i = 1 - fromIntegral i / fromIntegral (length ps - 1)
                  a0 = returnV $ v3 (fst $ ps V.! 0) az
                  b0 = returnV $ v3 (snd $ ps V.! 0) (zAB (1 :: Int))
                  c0 = returnV $ v3 (fst $ ps V.! 1) cz
                  n0 = normalize $ case axis of
                    X -> cross (b0 - a0) (c0 - a0)
                    Y -> cross (c0 - a0) (b0 - a0)
               in ( case axis of
                      Y ->
                        V.empty
                          `V.snoc` Vertex a0 n0 (V2 0 1)
                          `V.snoc` Vertex b0 n0 (V2 0 1)
                      X ->
                        V.empty
                          `V.snoc` Vertex b0 n0 (V2 0 1)
                          `V.snoc` Vertex a0 n0 (V2 0 1)
                  )
                    V.++ V.concatMap
                      ( \(i, cl0, cl1) ->
                          let a1 = returnV $ v3 (fst cl0) (zAB i)
                              c1 = returnV $ v3 (fst cl1) (zAB (i + 1))
                              b1 = returnV $ v3 (snd cl0) (zCD i)
                              d1 = returnV $ v3 (snd cl1) (zCD (i + i))
                              n = normalize $ case axis of
                                X -> cross (b1 - a1) (c1 - a1)
                                Y -> cross (c1 - a1) (b1 - a1)
                           in case axis of
                                Y ->
                                  V.empty
                                    `V.snoc` Vertex c1 n (V2 0 (v i))
                                    `V.snoc` Vertex d1 n (V2 1 (v i))
                                X ->
                                  V.empty
                                    `V.snoc` Vertex d1 n (V2 1 (v i))
                                    `V.snoc` Vertex c1 n (V2 0 (v i))
                      )
                      (V.izipWith (,,) ps (V.tail ps))
      where
        returnV v =
          splined3
            sps
            v
            ^. _xyz
              * unit

boardZ :: (HasCallStack) => V3 Float -> MapSplines -> (V2 Float, V2 Float) -> V2 Int -> Float -> Float -> Float -> Vector Vertex
boardZ unit sps segment divs az bz height =
  let tops = lineStrip unit sps segment divs az bz
      v i = fromIntegral i / fromIntegral (length tops - 1)
   in ( if length tops < 2
          then V.empty
          else
            let a = tops V.! 0
                b = a & _y -~ height
                c = tops V.! 1
                n = normalize $ cross (b - a) (c - a)
                tail1 = repeatUntilLengthsMatch (length tops) (V.tail tops)
                tail2 = repeatUntilLengthsMatch (length tops) (V.tail tail1)
             in V.empty
                  `V.snoc` Vertex a n (V2 0 0)
                  `V.snoc` Vertex b n (V2 0 1)
                  V.++ V.concatMap
                    ( \(i, t1, (t2, t3)) ->
                        let a = t1
                            c = t2
                            d = c & _y -~ height
                            e = t3
                            n1 = cross (a - c) (d - c)
                            n2 = cross (d - c) (e - c)
                            n = normalize $ n1 + n2
                         in V.empty
                              `V.snoc` Vertex c n (V2 (v (i + 1)) 1)
                              `V.snoc` Vertex d n (V2 (v (i + 1)) 0)
                    )
                    (V.izipWith (,,) tops (V.zip tail1 tail2))
      )

lineStrip :: V3 Float -> SplinePairs Float -> (V2 Float, V2 Float) -> V2 Int -> Float -> Float -> Vector (V3 Float)
lineStrip unit sps (range_1, range_2) (V2 divX divY) az bz =
  let isLinearAxis = uncurry (&&) (both (V.all (uncurry (&&) . both isLinear)) sps)
      xs = interval divX (range_1 ^. _x) (range_2 ^. _x)
      ys = interval divY (range_1 ^. _y) (range_2 ^. _y)
      n = max (length xs) (length ys)
      rs =
        V.zipWith
          V2
          (repeatUntilLengthsMatch n xs)
          (repeatUntilLengthsMatch n ys)
      z n = az + ((bz - az) / fromIntegral (length rs - 1)) * fromIntegral n
   in ( if
          | length ys >= 2 && isLinearAxis ->
              let a = v3 (V.head rs) az
                  b = v3 (V.last rs) (z (1 :: Int))
               in V.empty
                    `V.snoc` returnV a
                    `V.snoc` returnV b
          | length rs < 2 -> V.empty
          | otherwise ->
              V.concatMap
                ( \(i, y0, y1) ->
                    let a = returnV $ v3 y0 (z i)
                        b = returnV $ v3 y1 (z (i + 1))
                     in V.empty
                          `V.snoc` a
                          `V.snoc` b
                )
                (V.izipWith (,,) rs (V.tail rs))
      )
  where
    returnV v =
      splined3
        sps
        ( v ^. _xyz
        )
        * unit

v3 :: (R2 t) => t a -> a -> V3 a
v3 v2 z = V3 (v2 ^. _x) z (v2 ^. _y)
