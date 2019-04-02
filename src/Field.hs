module Field where

import Graphics.Gloss


type Index    = Int             -- ^ Cell の Index
type Position = (Int, Int)      -- ^ Field 内の Cell の位置

data Field = Field { width    :: Int   -- ^ Field の幅 (Cell 数)
                   , height   :: Int   -- ^ Field の高さ (Cell 数)
                   , cellSize :: Float -- ^ Cell の大きさ
                   } deriving Show


posToIndex :: Field -> Position -> Index
posToIndex field (x, y) = x + y * width field

indexToPos :: Field -> Index -> Position
indexToPos field i = let (y, x) = divMod i (width field) in (x, y)

posToGlossPoint :: Field -> Position -> Point
posToGlossPoint field (x, y) = (f x (width field), f y (height field))
  where f a b = let [a', b'] = map fromIntegral [a, b]
                in  (2 * a' - b' + 1) * cellSize field / 2

indexToGlossPoint :: Field -> Index -> Point
indexToGlossPoint field i = posToGlossPoint field $ indexToPos field i

neighborhood :: Field -> Position -> [(Int, Int)] -> [Position]
neighborhood field (x, y) lst = [(mod (x + a) w, mod (y + b) h) | (a, b) <- lst]
  where (w, h) = (width field, height field)

vonNeumannN :: Field -> Position -> [Position]
vonNeumannN field (x, y) = neighborhood field (x, y) lst
  where lst = [(0, -1), (0, 1), (-1, 0), (1, 0)]

mooreN :: Field -> Position -> [Position]
mooreN field (x, y) = neighborhood field (x, y) lst
  where
    lst = [(a, b) | a <- ns, b <- ns, not (a == 0 && b == 0)]
    ns  = [-1, 0, 1]
