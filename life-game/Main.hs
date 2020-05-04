-------------------------------------------------------------
-- |
--   Module      : Main
--   Description : Life Game
--   Copyright   : (c) little Haskeller, 2019
--   License     : BSD3
--
--   Gloss を使った Life Game
--
-------------------------------------------------------------

module Main where


import           Control.Applicative
import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as VU
import           Field
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           System.Environment
import           System.Random


type Model = VU.Vector Bool


-----------------------------------------
-- * パラメータ
-----------------------------------------

width  = 500 :: Int             -- 横の Cell 数
height = 250 :: Int             -- 縦の Cell 数
size   =   3 :: Float           -- Cell のサイズ


-----------------------------------------
-- * Life-Game
-----------------------------------------

main :: IO ()
main = do
  args <- getArgs
  let fd = initField width height size moore
  cells <- if null args
           then VU.replicateM (width * height) (randomIO :: IO Bool)
           else initCells fd $ head args
  simulate (window fd) black 20 cells (drawModel fd) (simCells fd)
    where window fd = InWindow "Life Game" (windowSize fd) (0, 0)

-- | Cell の初期化
initCells :: Field -> FilePath -> IO Model
initCells fd path = do
  (pos : text) <- lines <$> readFile path
  return ((VU.replicate (width * height) False) VU.// (g (read pos) text))
    where
      g (x, y) css = concatMap (h [x ..]) $ zip [y ..] css
      h is (j, cs) = [(posToIndex fd (i, j), conv c) | (i, c) <- zip is cs]
      conv c = if c == '#' then True else False

-- | モデルを図形に変換する関数
drawModel :: Field -> Model -> Picture
drawModel fd cells = Pictures $ V.toList $ V.imap drawCell $ V.convert cells
  where drawCell i cell = if cell then indexToDrawCell fd i cyan else Blank

-- | モデルを更新する関数
simCells :: Field -> ViewPort -> Float -> Model -> Model
simCells fd _ _ cells = VU.imap check cells
  where
    check i bool
      | bool      = if (cellNum == 2 || cellNum == 3) then True else False
      | otherwise = if (cellNum == 3)                 then True else False
      where
        neighborLst = (neighborTbl fd) V.! i
        cellNum     = sum [if cells VU.! j then 1 else 0 | j <- neighborLst]
