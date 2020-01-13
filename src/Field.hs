-------------------------------------------------------------
-- |
--   Module      : Field
--   Description : Cellular Automaton on Gloss
--   Copyright   : (c) little Haskeller, 2019
--   License     : BSD3
--
--   Gloss を使った Cellular Automaton
--
-------------------------------------------------------------

module Field where

import qualified Data.Vector    as V
import           Graphics.Gloss


type Index    = Int             -- ^ Cell の Index
type Position = (Int, Int)      -- ^ Field 内の Cell の位置

data Field = Field { cellSize          :: Float
                   , pointTable        :: V.Vector Point
                   , neighborhoodTable :: V.Vector [Index]
                   } deriving Show



-----------------------------------------
-- * 初期化
-----------------------------------------

-- | Field の初期化
initField :: Int -> Int -> Float -> (Int -> Int -> Position -> [Index]) -> Field
initField width height size nbFunc =
  Field { cellSize          = size
        , pointTable        = V.map (posToPoint width height size) positionT
        , neighborhoodTable = V.map (nbFunc width height) positionT
        }
  where positionT = V.generate (width * height) (indexToPos width)

-- | Window のサイズ
windowSize :: Int -> Int -> Float -> (Int, Int)
windowSize width height size = (width * ics, height * ics)
  where ics = truncate size



-----------------------------------------
-- * 変換
-----------------------------------------

-- | Index -> Position
indexToPos :: Int -> Index -> Position
indexToPos width i = let (y, x) = divMod i width in (x, y)

-- | Position -> Index
posToIndex :: Int -> Position -> Index
posToIndex width (x, y) = x + y * width

-- | Position -> Point
posToPoint :: Int -> Int -> Float -> Position -> Point
posToPoint width height size (x, y) = (x', y')
  where
    [w, h, xx, yy] = map fromIntegral [width, height, x, y]
    x' = (xx - w / 2) * size
    y' = (h / 2 - yy) * size



-----------------------------------------
-- * 表示
-----------------------------------------

-- | Cell の描画 : Index -> Cell
indexToDrawCell :: Field -> Index -> Color -> Picture
indexToDrawCell field i col = Color col pic
  where
    pic = Polygon [(x0, y0), (x1, y0), (x1, y1), (x0, y1)]
    (x0, y0) = (pointTable field) V.! i
    (x1, y1) = (x0 + cellSize field - 1, y0 - cellSize field + 1)



-----------------------------------------
-- * 近傍のインデックスのリスト
-----------------------------------------

-- | Von Neumann 近傍
--
--   0
-- 3 x 1
--   2
--
neumann :: Int -> Int -> Position -> [Index]
neumann width height (x, y) = neighborhood width height (x, y) lst
  where lst = [(0, -1), (1, 0), (0, 1), (-1, 0)]

-- | Moore 近傍
--
-- 7 0 1
-- 6 x 2
-- 5 4 3
--
moore :: Int -> Int -> Position -> [Index]
moore width height (x, y) = neighborhood width height (x, y) lst
  where lst = [ (0, -1), (1, -1), (1, 0), (1, 1)
              , (0, 1), (-1, 1), (-1, 0), (-1, -1) ]

-- | 近傍の計算
neighborhood :: Int -> Int -> Position -> [(Int, Int)] -> [Index]
neighborhood width height (x, y) lst = map f lst
  where f (a, b) = posToIndex width $ (mod (x + a) width, mod (y + b) height)
