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
import           Data.Semigroup               ((<>))
import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as VU
import           Field
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Options.Applicative
import           System.Environment
import           System.Random


type Model = VU.Vector Bool


---------------------------------------------------
-- * パラメータ
---------------------------------------------------

width  = 300 :: Int   -- ^ 横の Cell 数
height = 200 :: Int   -- ^ 縦の Cell 数
size   =   4 :: Float -- ^ Cell のサイズ
speed  =  15 :: Int   -- ^ 描画スピード


---------------------------------------------------
-- * オプション
---------------------------------------------------

data Option = Option
    { file :: String
    , w    :: Int
    , h    :: Int
    , s    :: Int
    }

opt :: Parser Option
opt = Option
  <$> strOption   (short 'f' <> value "")
  <*> option auto (short 'w' <> value width)
  <*> option auto (short 'h' <> value height)
  <*> option auto (short 's' <> value speed)


---------------------------------------------------
-- * Life Game
---------------------------------------------------

main :: IO ()
main = do
  Option file w h s <- execParser (info opt mempty)
  let fd = initField w h size moore
  cells <- initCells fd file
  simulate (window fd) black s cells (drawModel fd) (simCells fd)
    where window fd = InWindow "Life Game" (windowSize fd) (0, 0)

-- | Cell の初期化
initCells :: Field -> FilePath -> IO Model
initCells fd path = if null path
                    then VU.replicateM (w * h) randomIO
                    else do
  (pos : text) <- lines <$> readFile path
  return ((VU.replicate (w * h) False) VU.// lst pos)
    where
      (w, h) = (fieldWidth fd, fieldHeight fd)
      lst pos = let (x, y) = read pos in concatMap (f [x ..]) $ zip [y ..] text
      f is (j, cs) = [(posToIndex fd (i, j), conv c) | (i, c) <- zip is cs]
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
