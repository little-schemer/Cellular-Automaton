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

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU
import           Graphics.Gloss


type Index    = Int             -- ^ Cell の Index
type Position = (Int, Int)      -- ^ Field 内の Cell の位置

data Field = Field { cellSize          :: Float
                   -- , positionTable     :: VU.Vector Position
                   , pointTable        :: VU.Vector Point
                   , neighborhoodTable :: V.Vector ([Index], [Index])
                   } deriving Show



-----------------------------------------
-- * 初期化
-----------------------------------------

-- | Field の初期化
initField :: Int -> Int -> Float -> Field
initField width height size =
  Field { cellSize          = size
        -- , positionTable     = positionT
        , pointTable        = VU.map (posToPoint width height size) positionT
        , neighborhoodTable = V.map (neighborhood width height) $ VU.convert positionT
        }
  where positionT = VU.generate (width * height) (indexToPos width)

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
    (x0, y0) = (pointTable field) VU.! i
    (x1, y1) = (x0 + cellSize field - 1, y0 - cellSize field + 1)



-----------------------------------------
-- * 近傍
-----------------------------------------

-- | 近傍のインデックスのリスト
--
-- 7 0 4
-- 3 x 1
-- 6 2 5
--
neighborhood :: Int -> Int -> Position -> ([Index], [Index])
neighborhood width height (x, y) = (neumann, moore)
  where
    f (a, b) = posToIndex width $ (mod (x + a) width, mod (y + b) height)
    neumann = map f [(0, -1), (1, 0), (0,  1), (-1,  0)]
    moore   = neumann ++ (map f [(1, -1), (1, 1), (-1, 1), (-1, -1)])
