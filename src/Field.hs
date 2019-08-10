module Field where

import Graphics.Gloss


type Index    = Int             -- ^ Cell の Index
type Position = (Int, Int)      -- ^ Field 内の Cell の位置

data Field = Field { ix :: Int   -- ^ 横の Cell 数 (Int)
                   , iy :: Int   -- ^ 縦の Cell 数 (Int)
                   , fx :: Float -- ^ 横の Cell 数 (Float)
                   , fy :: Float -- ^ 縦の Cell 数 (Float)
                   , cs :: Float -- ^ Cell の大きさ
                   , msg :: Float -- ^ message 表示部分の高さ
                   , rpX :: Float -- ^ 基準点の x 座標
                   , rpY :: Float -- ^ 基準点の y 座標
                   } deriving Show


---------------------------------------------------
-- * 初期化
---------------------------------------------------

-- | Field の初期化
initField :: Int -> Int -> Float -> Field
initField width height cellSize =
  Field { ix = width
        , iy = height
        , fx = xx
        , fy = yy
        , cs = cellSize
        , msg = msgLine
        , rpX = - xx * cellSize / 2
        , rpY = - (yy * cellSize - msgLine) / 2 }
  where
    [xx, yy] = map fromIntegral[width, height]
    msgLine = 25

-- | Window のサイズ
windowSize :: Field -> (Int, Int)
windowSize field = (ix field * ics, iy field * ics + imsg)
  where [ics, imsg] = map truncate [cs field, msg field]


---------------------------------------------------
-- * 表示
---------------------------------------------------

-- | Cell の描画 : Index -> Cell
indexToDrawCell :: Field -> Int -> Picture
indexToDrawCell field i = Polygon [(x, y), (x + s, y), (x + s, y + s), (x, y + s)]
  where
    s = cs field - 1
    (x, y) = indexToGlossPoint field i

-- | Cell の描画 : Position -> Cell
posToDrawCell :: Field -> Position -> Picture
posToDrawCell field (x, y) =
  Polygon [(x', y'), (x' + s, y'), (x' + s, y' + s), (x', y' + s)]
  where
    s = cs field - 1
    (x', y') = posToGlossPoint field (x, y)

-- | メッセージの表示
dispMsg :: Field -> Color -> String -> Picture
dispMsg field clr str = Translate x y $ Scale 0.15 0.15 $ Color clr $ Text str
  where (x, y) = (rpX field + 5, rpY field - msg field + 5)


---------------------------------------------------
-- * Index <--> Position
---------------------------------------------------

-- | Position -> Index
posToIndex :: Field -> Position -> Index
posToIndex field (x, y) = x + y * ix field

-- | Index -> Position
indexToPos :: Field -> Index -> Position
indexToPos field i = let (y, x) = divMod i (ix field) in (x, y)

-- | Position -> Gloss の座標
posToGlossPoint :: Field -> Position -> Point
posToGlossPoint field (x, y) = (rpX field + xx * cs field, rpY field + yy * cs field)
  where [xx, yy] = map fromIntegral [x, y]

-- | Index -> Gloss の座標
indexToGlossPoint :: Field -> Index -> Point
indexToGlossPoint field i = posToGlossPoint field $ indexToPos field i


---------------------------------------------------
-- * 近傍の位置
---------------------------------------------------
neighborhood :: Field -> Position -> [(Int, Int)] -> [Position]
neighborhood field (x, y) lst = [(mod (x + a) w, mod (y + b) h) | (a, b) <- lst]
  where (w, h) = (ix field, iy field)

-- | フォン・ノイマン近傍
--
--       0
--    3     1
--       2
--
vonNeumannN :: Field -> Position -> [Position]
vonNeumannN field (x, y) = neighborhood field (x, y) lst
  where lst = [(0, 1), (1, 0), (0, -1), (-1, 0)]


-- | ムーア近傍
--
--   7 0 1
--   6   2
--   5 4 3
--
mooreN :: Field -> Position -> [Position]
mooreN field (x, y) = neighborhood field (x, y) lst
  where lst = [ (0, 1), (1, 1), (1, 0), (1, -1)
              , (0, -1) , (-1, -1), (-1, 0), (-1, 1)]
