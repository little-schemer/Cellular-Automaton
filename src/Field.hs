-------------------------------------------------------------
-- |
--   Module      : Field
--   Description : Cellular Automaton on Gloss
--   Copyright   : (c) little Haskeller, 2019, 2020
--   License     : BSD3
--
--   Gloss を使った Cellular Automaton
--
-------------------------------------------------------------

module Field where

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU
import           Graphics.Gloss


type Index    = Int             -- ^ Cell の Index
type Position = (Int, Int)      -- ^ Field 内の Cell の位置

data Field = Field
    { fieldWidth  :: Int
    , fieldHeight :: Int
    , cellSize    :: Float
    , cellPicture :: Picture
    , pointTbl    :: VU.Vector Point
    , neighborTbl :: V.Vector [Index]
    }
    deriving Show


---------------------------------------------------
-- * 初期化
---------------------------------------------------

-- | Field の初期化
initField :: Int                                 -- ^ 横の Cell 数
          -> Int                                 -- ^ 縦の Cell 数
          -> Float                               -- ^ Cell の描画サイズ
          -> (Int -> Int -> Position -> [Index]) -- ^ 近傍を計算する関数
          -> Field
initField width height size nbFunc = Field width height size pic pTbl nTbl
  where
    pic    = rectangleSolid (size - 1) (size - 1)
    -- pic    = circleSolid (size / 2 - 0.1)
    posTbl = V.generate (width * height) (indexToPos' width)
    pTbl   = VU.map (posToPoint' width height size) $ V.convert posTbl
    nTbl   = V.map (nbFunc width height) posTbl

windowSize :: Field -> (Int, Int)
windowSize fd = (fieldWidth fd * ics, fieldHeight fd * ics)
  where ics = truncate (cellSize fd)


---------------------------------------------------
-- * 変換
---------------------------------------------------

-- | Index -> Position
indexToPos :: Field -> Index -> Position
indexToPos fd i = indexToPos' (fieldWidth fd) i

indexToPos' :: Int -> Index -> Position
indexToPos' width i = let (y, x) = divMod i width in (x, y)

-- | Position -> Index
posToIndex :: Field -> Position -> Index
posToIndex fd (x, y) = posToIndex' (fieldWidth fd) (x, y)

posToIndex' :: Int -> Position -> Index
posToIndex' width (x, y) = x + y * width

-- | Position -> Gloss Point
posToPoint :: Field -> Position -> Point
posToPoint fd (x, y) = posToPoint' w h s (x, y)
  where (w, h, s) = (fieldWidth fd, fieldHeight fd, cellSize fd)

posToPoint' :: Int -> Int -> Float -> Position -> Point
posToPoint' width height size (x, y) = (x', y')
  where
    [w, h, xx, yy] = map fromIntegral [width, height, x, y]
    (x', y') = ((xx - w / 2 + 0.5) * size, (h / 2 - yy - 0.5) * size)


---------------------------------------------------
-- * 表示
---------------------------------------------------

-- | Cell の描画
indexToDrawCell :: Field -> Index -> Color -> Picture
indexToDrawCell fd i c = Translate x y $ Color c $ cellPicture fd
  where (x, y) = (pointTbl fd) VU.! i


---------------------------------------------------
-- * 近傍のインデックスのリスト
---------------------------------------------------

-- | 近傍の計算
neighborhood :: [(Int, Int)] -> Int -> Int -> Position -> [Index]
neighborhood lst width height (x, y) = map f lst
  where f (a, b) = posToIndex' width $ (mod (x + a) width, mod (y + b) height)

-- | Von Neumann 近傍
--
--   0
-- 3 x 1
--   2
--
neumann :: Int -> Int -> Position -> [Index]
neumann = neighborhood [(0, -1), (1, 0), (0, 1), (-1, 0)]

-- | Moore 近傍
--
-- 7 0 1
-- 6 x 2
-- 5 4 3
--
moore :: Int -> Int -> Position -> [Index]
moore = neighborhood [ ( 0, -1), ( 1, -1), ( 1,  0), ( 1,  1)
                     , ( 0,  1), (-1,  1), (-1,  0), (-1, -1) ]
