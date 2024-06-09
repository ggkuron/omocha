module Omocha.Shape
  ( plane,
    cube,
    cylinder,
    slope,
    cone,
    rect,
    interval,
    splined2,
    splined3,
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
import RIO hiding (trace, traceShow, traceStack)
import Prelude (tail)

plane :: V3 Float -> V3 Float -> V3 Float -> V4 Float -> Splines Float -> V2 Int -> Vector Mesh
plane unit (V3 w d h) offset color sps divs =
  let vs = axesedBoardXZ unit sps ((0, w), (0, h)) divs d d d d
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

-- heightが一定
cube :: V3 Float -> V3 Float -> V3 Float -> V4 Float -> Splines Float -> V2 Int -> Vector Mesh
cube unit (V3 w d h) = cube' unit (V2 w h) d d d d

slope :: V3 Float -> TipEdge -> (Float, (Float, Float), Float) -> V3 Float -> V4 Float -> Splines Float -> V2 Int -> Vector Mesh
slope unit edge (w, (high, low), h) offset color sps divs =
  let (a, b, c, d) = case edge of
        RowMin -> (high, low, low, high)
        RowMax -> (low, high, high, low)
        ColumnMin -> (high, high, low, low)
        ColumnMax -> (low, low, high, high)
   in cube' unit (V2 w h) a b c d offset color sps divs

-- heightが一定でない
cube' :: V3 Float -> V2 Float -> Float -> Float -> Float -> Float -> V3 Float -> V4 Float -> Splines Float -> V2 Int -> Vector Mesh
cube' unit (V2 w h) az bz cz dz offset color sps divs =
  let top = axesedBoardXZ unit sps ((0, w), (0, h)) divs az bz cz dz
      bottom = axesedBoardXZ unit sps ((0, w), (h, 0)) divs 0 0 0 0
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

cylinder :: V3 Float -> Maybe EdgePoint -> V3 Float -> V3 Float -> V4 Float -> Splines Float -> V2 Int -> Vector Mesh
cylinder unit center size@(V3 w d h) offset color sps@(spx, spy) (V2 divX divY) =
  let halfSize = size / 2
      depth = d
      (r, offset') = case center of
        Nothing -> ((size ^. _xz) / 2, V3 (halfSize ^. _x) 0 (halfSize ^. _z))
        Just ep -> (size ^. _xz, (\(a, b) -> V3 a 0 b) (both fromBool ep) * size)
      (rstart, rend) = case center of
        Nothing -> (0, (-2) * pi)
        Just (False, False) -> (pi * 0.5, 0)
        Just (False, True) -> (pi * 2, pi * 1.5)
        Just (True, False) -> (pi, pi * 0.5)
        Just (True, True) -> (pi * 1.5, pi)
      topPoints = ellipticalCircle r depth rstart rend
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
                `V.snoc` axesedBoardY unit (calcSplines spx 0 ^. _x) Y spy (h, 0) divY depth depth depth
                `V.snoc` axesedBoardY unit (calcSplines spy 0 ^. _x) X spx (w, 0) divX depth depth depth
            Just (False, True) ->
              V.empty
                `V.snoc` axesedBoardY unit (calcSplines spx 0 ^. _x) Y spy (h, 0) divY depth depth depth
                `V.snoc` axesedBoardY unit (calcSplines spy h ^. _x) X spx (0, w) divX depth depth depth
            Just (True, False) ->
              V.empty
                `V.snoc` axesedBoardY unit (calcSplines spx w ^. _x) Y spy (0, h) divY depth depth depth
                `V.snoc` axesedBoardY unit (calcSplines spy 0 ^. _x) X spx (w, 0) divX depth depth depth
            Just (True, True) ->
              V.empty
                `V.snoc` axesedBoardY unit (calcSplines spx w ^. _x) Y spy (0, h) divY depth depth depth
                `V.snoc` axesedBoardY unit (calcSplines spy h ^. _x) X spx (0, w) divX depth depth depth
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

interval :: (Ord a, Num a, Enum a, Fractional a) => a -> a -> a -> Vector a
interval _ start end | start == end = V.singleton start
interval step start end
  | start > end && step < 0 = start `V.cons` V.unfoldr (gen (>)) (Just 1 :: Maybe Int)
  | start < end && step > 0 = start `V.cons` V.unfoldr (gen (<)) (Just 1 :: Maybe Int)
  where
    gen cmp (Just n) =
      let x = start + fromIntegral n * step
       in Just (if x `cmp` end then (x, Just (n + 1)) else (end, Nothing))
    gen _ Nothing = Nothing
interval _ start end = V.empty `V.snoc` start `V.snoc` end

ellipticalCircle :: (Enum a, Ord a, Floating a, R2 t) => t a -> a -> a -> a -> Vector (V3 a)
ellipticalCircle r y start end =
  V.map (\t -> V3 (r ^. _x * cos t) y (r ^. _y * sin t)) $ interval step start end
  where
    step = if start > end then -0.25 else 0.25

cone :: V3 Float -> Maybe EdgePoint -> V3 Float -> V3 Float -> V4 Float -> Splines Float -> Vector Mesh
cone unit center size@(V3 _ depth _) offset color sps =
  let r = (size ^. _xz) / 2
      t' =
        unit
          * splined3
            sps
            ( case center of
                Nothing -> v3 r depth
                Just ep -> ((\(a, b) -> V3 a 0 b) (both fromBool ep) * size) & _y .~ depth
            )
      c = size / 2 & _y .~ 0
      c' = unit * splined3 sps c
      bottomPoints = V.toList . V.map (+ c) $ ellipticalCircle r 0 (-(2 * pi)) 0
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

boardP' :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> V3 Float -> [Vertex]
boardP' unit a b c d =
  let n = normalize $ cross (b - a) (c - a)
   in [ Vertex (a * unit) n (V2 0 1),
        Vertex (b * unit) n (V2 0 0),
        Vertex (c * unit) n (V2 1 1),
        Vertex (d * unit) n (V2 1 0)
      ]

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

zipPair :: (a, b) -> (c, d) -> ((a, c), (b, d))
zipPair (a, b) (c, d) = ((a, c), (b, d))

zipV2 :: (a, a) -> V2 b -> ((a, b), (a, b))
zipV2 (a, b) (V2 c d) = ((a, c), (b, d))

type AxisPair = (Vector (Spline Float), Segment)

type Segment = ((Float, Float), Int)

axesedBoardXZ :: V3 Float -> Splines Float -> ((Float, Float), (Float, Float)) -> V2 Int -> Float -> Float -> Float -> Float -> Vector (Vector Vertex)
axesedBoardXZ unit sps range divs az bz cz dz =
  let (isLinearX, isLinearY) = both (all isLinear) sps
      spPair@(spxPair, spyPair) = zipPair sps (zipV2 range divs)
   in if
        | isLinearX && isLinearY ->
            let ((x0, x1), (y0, y1)) =
                  both
                    (\((sps, ((f, t), _)) :: AxisPair) -> both (calcSplines sps . fromIntegral) (ceiling f :: Int, floor t :: Int))
                    spPair
                a = x0 + y0 ^. _yx
                b = x0 + y1 ^. _yx
                c = x1 + y0 ^. _yx
                d = x1 + y1 ^. _yx
             in V.singleton $ V.fromList $ boardP' unit (v3 a az) (v3 b bz) (v3 c cz) (v3 d dz)
        | isLinearX -> V.singleton $ axesedDivisions unit X spxPair spyPair az bz cz dz
        | isLinearY -> V.singleton $ axesedDivisions unit Y spyPair spxPair az cz bz dz
        | otherwise ->
            let ys = vs spyPair
             in V.concatMap
                  ( \(i, y0, y1) ->
                      let zAB n = az + ((bz - az) / fromIntegral (length ys - 1)) * fromIntegral n
                          zCD n = cz + ((dz - cz) / fromIntegral (length ys - 1)) * fromIntegral n
                       in V.singleton $ axesedDivisions unit X spxPair (fst spyPair, ((y0 ^. _x, y1 ^. _x), 4)) (zAB i) (zAB (i + 1)) (zCD i) (zCD (i + 1))
                  )
                  (V.izipWith (,,) ys (V.tail ys))
  where
    axesedDivisions :: V3 Float -> Axis -> AxisPair -> AxisPair -> Float -> Float -> Float -> Float -> Vector Vertex
    axesedDivisions unit axis mainPair crossPair az bz cz dz =
      let (sp, r) = fst <$> mainPair
       in if null sp
            then V.empty
            else
              let (start, end) = both (calcSplines sp) r
                  ys = vs crossPair
               in if length ys < 2
                    then V.empty
                    else
                      let zAB n = az + ((bz - az) / fromIntegral (length ys - 1)) * fromIntegral n
                          zCD n = cz + ((dz - cz) / fromIntegral (length ys - 1)) * fromIntegral n
                          ac = end - start
                          v i = 1 - fromIntegral i / fromIntegral (length ys - 1)
                          a0 = v3 (start + ys V.! 0 ^. _yx) az
                          b0 = v3 (start + ys V.! 1 ^. _yx) (zAB (1 :: Int))
                          c0 = v3 (start + ac + ys V.! 0 ^. _yx) cz
                          n0 = normalize $ cross (b0 - a0) (c0 - a0)
                       in ( case axis of
                              Y ->
                                V.empty
                                  `V.snoc` Vertex (returnV a0) n0 (V2 0 1)
                                  `V.snoc` Vertex (returnV c0) n0 (V2 0 1)
                              X ->
                                V.empty
                                  `V.snoc` Vertex (returnV c0) n0 (V2 0 1)
                                  `V.snoc` Vertex (returnV a0) n0 (V2 0 1)
                          )
                            V.++ V.concatMap
                              ( \(i, y0, y1) ->
                                  let a0 = v3 (start + y0 ^. _yx) (zAB i)
                                      d1 = v3 (start + ac + y1 ^. _yx) (zCD i)
                                      b1 = v3 (start + y1 ^. _yx) (zAB (i + 1))
                                      n = normalize $ cross (d1 - b1) (a0 - b1)
                                   in case axis of
                                        Y ->
                                          V.empty
                                            `V.snoc` Vertex (returnV b1) n (V2 0 (v i))
                                            `V.snoc` Vertex (returnV d1) n (V2 1 (v i))
                                        X ->
                                          V.empty
                                            `V.snoc` Vertex (returnV d1) n (V2 1 (v i))
                                            `V.snoc` Vertex (returnV b1) n (V2 0 (v i))
                              )
                              (V.izipWith (,,) ys (V.tail ys))
      where
        returnV v =
          ( case axis of
              Y -> v ^. _zyx
              X -> v ^. _xyz
          )
            * unit

axisValue :: Axis -> Float -> V3 Float
axisValue X v = V3 v 0 0
axisValue Y v = V3 0 0 v

crossAxis :: Axis -> Axis
crossAxis X = Y
crossAxis Y = X

-- height(crossOffset)は一定を仮定
axesedBoardY :: V3 Float -> Float -> Axis -> Vector (Spline Float) -> (Float, Float) -> Int -> Float -> Float -> Float -> Vector Vertex
axesedBoardY unit crossOffset axis sp range divs az bz height =
  let tops = axesedLineStrip unit axis sp range divs az bz
      v i = fromIntegral i / fromIntegral (length tops - 1)
      offset = axisValue (crossAxis axis) crossOffset
   in if length tops < 2
        then V.empty
        else
          let t1 = tops V.! 0 + offset
              t2 = tops V.! 1 + offset
              a = t1
              b = t1 & _y -~ height
              c = t2
              n = normalize $ cross (b - a) (c - a)
           in ( case axis of -- 媒介変数(range)の増加方向指定が軸方向を向くように並べる
                  X ->
                    V.empty
                      `V.snoc` Vertex a n (V2 0 0)
                      `V.snoc` Vertex b n (V2 0 1)
                  Y ->
                    V.empty
                      `V.snoc` Vertex b n (V2 0 1)
                      `V.snoc` Vertex a n (V2 0 0)
              )
                V.++ V.concatMap
                  ( \(i, t1, t2) ->
                      let a = t1 + offset
                          b = a & _y -~ height
                          c = t2 + offset
                          d = c & _y -~ height
                          n = normalize $ cross (b - a) (c - a)
                       in case axis of
                            X ->
                              V.empty
                                `V.snoc` Vertex c n (V2 (v (i + 1)) 1)
                                `V.snoc` Vertex d n (V2 (v (i + 1)) 0)
                            Y ->
                              V.empty
                                `V.snoc` Vertex d n (V2 (v (i + 1)) 0)
                                `V.snoc` Vertex c n (V2 (v (i + 1)) 1)
                  )
                  (V.izipWith (,,) tops (V.tail tops))

axesedLineStrip :: V3 Float -> Axis -> Vector (Spline Float) -> (Float, Float) -> Int -> Float -> Float -> Vector (V3 Float)
axesedLineStrip unit axis sp range divs az bz =
  let isLinearAxis = V.all isLinear sp
      ys = vs (sp, (range, divs))
      z n = az + ((bz - az) / fromIntegral (length ys - 1)) * fromIntegral n
   in ( if
          | length ys > 2 && isLinearAxis ->
              let a = v3 (V.head ys) az
                  b = v3 (V.last ys) (z (1 :: Int))
               in V.empty
                    `V.snoc` returnV a
                    `V.snoc` returnV b
          | length ys < 2 -> V.empty
          | otherwise ->
              V.concatMap
                ( \(i, y0, y1) ->
                    let a = v3 (y0 ^. _yx) (z i)
                        b = v3 (y1 ^. _yx) (z (i + 1))
                     in V.empty
                          `V.snoc` returnV a
                          `V.snoc` returnV b
                )
                (V.izipWith (,,) ys (V.tail ys))
      )
  where
    returnV v =
      ( case axis of
          Y -> v ^. _zyx
          X -> v ^. _xyz
      )
        * unit

vs :: AxisPair -> Vector (V2 Float)
vs (sp, ((a, b), _)) | a == b = V.singleton $ calcSplines sp a
vs (sp, ((a, b), divs)) = V.map (calcSplines sp) (interval ((b - a) / fromIntegral divs) a b)

v3 :: (R2 t) => t a -> a -> V3 a
v3 v2 z = V3 (v2 ^. _x) z (v2 ^. _y)
