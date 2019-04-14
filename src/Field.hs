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
initField width height cellSize = Field { ix = width
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
windowSize fd = (ix fd * ics, iy fd * ics + imsg)
  where [ics, imsg] = map truncate [cs fd, msg fd]


---------------------------------------------------
-- * 表示
---------------------------------------------------

-- | Cell の描画
indexToDrawCell :: Field -> Int -> Picture
indexToDrawCell fd i = Polygon [(x, y), (x + s, y), (x + s, y + s), (x, y + s)]
  where
    s = cs fd - 1
    (x, y) = indexToGlossPoint fd i

-- | メッセージの表示
dispMsg :: Field -> Color -> String -> Picture
dispMsg fd clr str = Translate x y $ Scale 0.15 0.15 $ Color clr $ Text str
  where (x, y) = (rpX fd + 5, rpY fd - msg fd + 5)


---------------------------------------------------
-- * Index <--> Position
---------------------------------------------------

-- | Position -> Index
posToIndex :: Field -> Position -> Index
posToIndex fd (x, y) = x + y * ix fd

-- | Index -> Position
indexToPos :: Field -> Index -> Position
indexToPos fd i = let (y, x) = divMod i (ix fd) in (x, y)

-- | Position -> Gloss の座標
posToGlossPoint :: Field -> Position -> Point
posToGlossPoint fd (x, y) = (rpX fd + xx * cs fd, rpY fd + yy * cs fd)
  where [xx, yy] = map fromIntegral [x, y]

-- | Index -> Gloss の座標
indexToGlossPoint :: Field -> Index -> Point
indexToGlossPoint fd i = posToGlossPoint fd $ indexToPos fd i


---------------------------------------------------
-- * 近傍の位置
---------------------------------------------------
neighborhood :: Field -> Position -> [(Int, Int)] -> [Position]
neighborhood fd (x, y) lst = [(mod (x + a) w, mod (y + b) h) | (a, b) <- lst]
  where (w, h) = (ix fd, iy fd)

-- | フォン・ノイマン近傍
vonNeumannN :: Field -> Position -> [Position]
vonNeumannN fd (x, y) = neighborhood fd (x, y) lst
  where lst = [(0, -1), (0, 1), (-1, 0), (1, 0)]

-- | ムーア近傍
mooreN :: Field -> Position -> [Position]
mooreN fd (x, y) = neighborhood fd (x, y) lst
  where lst = [(a, b) | a <- [-1, 0, 1], b <- [-1, 0, 1], (a /= 0 || b /= 0)]
