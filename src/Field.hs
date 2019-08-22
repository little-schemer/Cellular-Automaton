module Field where

import Graphics.Gloss
import qualified Data.Vector as Vec


type Index = Int                -- ^ Cell の Index
type Position = (Int, Int)      -- ^ Field 内の Cell の位置

data Field = Field { cellSize :: Float
                   , positionTable :: Vec.Vector Position
                   , pointTable :: Vec.Vector Point
                   , neighborhoodTable :: Vec.Vector ([Index], [Index])
                   } deriving Show


-------------------------------------------------------------
-- * 初期化
-------------------------------------------------------------

-- | Field の初期化
initField :: Int -> Int -> Float -> Field
initField width height size = Field { cellSize      = size
                                    , positionTable = positionT
                                    , pointTable    = pointT
                                    , neighborhoodTable = neighborhoodT
                                    }
  where
    positionT = Vec.generate (width * height) (indexToPos width)
    pointT    = Vec.map (posToPoint width height size) positionT
    neighborhoodT = Vec.map (neighborhood width height) positionT

-- | Window のサイズ
windowSize :: Int -> Int -> Float -> (Int, Int)
windowSize width height size = (width * ics, height * ics)
  where ics = truncate size


-------------------------------------------------------------
-- * 変換
-------------------------------------------------------------
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
    x' = (xx - (w - 1) / 2) * size
    y' = ((h - 1) / 2 - yy) * size


-------------------------------------------------------------
-- * 表示
-------------------------------------------------------------
-- | Cell の描画 : Index -> Cell
indexToDrawCell :: Field -> Index -> Picture
indexToDrawCell field i = Polygon [(x, y), (x + s, y), (x + s, y + s), (x, y + s)]
  where
    (x, y) = (pointTable field) Vec.! i
    s      = cellSize field - 1


-------------------------------------------------------------
-- * 近傍の計算
-------------------------------------------------------------
neighborhood :: Int -> Int -> Position -> ([Index], [Index])
neighborhood width height pos = (neumann, moore')
  where
    posList (x, y) lst = [(mod (x + a) width, mod (y + b) height) | (a, b) <- lst]
    neumann = map (posToIndex width) $ posList pos [(0, -1), (1, 0), (0, 1), (-1, 0)]
    moore'  = map (posToIndex width) $ posList pos [(1, -1), (1, 1), (-1, 1), (-1, -1)]
