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


import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as VU
import           Field
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Options.Applicative
import           System.Random


type Model = VU.Vector Int


---------------------------------------------------
-- * パラメータ
---------------------------------------------------

width  = 300 :: Int   -- ^ 横の Cell 数
height = 200 :: Int   -- ^ 縦の Cell 数
size   =   3 :: Float -- ^ Cell のサイズ
speed  =  15 :: Int   -- ^ 描画スピード


---------------------------------------------------
-- * コマンドオプション
---------------------------------------------------

data Option = Option
    { file      :: String       -- ^ 読み込むファイル名
    , fdWidth   :: Int          -- ^ 横の Cell 数
    , fdHeight  :: Int          -- ^ 縦の Cell 数
    , drawSpeed :: Int          -- ^ 描画スピード
    }

opt :: Parser Option
opt = Option
  <$> strOption   (short 'f' <> value ""     <> metavar "String")
  <*> option auto (short 'w' <> value width  <> metavar "Int")
  <*> option auto (short 'h' <> value height <> metavar "Int")
  <*> option auto (short 's' <> value speed  <> metavar "Int")


---------------------------------------------------
-- * Life Game
---------------------------------------------------

main :: IO ()
main = do
  Option fn w h s <- execParser (info opt mempty)
  let fd = initField w h size moore
  cells <- initCells fd fn
  simulate (window fd) black s cells (drawModel fd) (simCells fd)
    where window fd = InWindow "Life Game" (windowSize fd) (0, 0)

-- | Cell の初期化
initCells :: Field -> FilePath -> IO Model
initCells fd path = if null path
                    then VU.replicateM (w * h) (randomRIO (0, 1))
                    else do
  (pos : css) <- lines <$> readFile path
  return ((VU.replicate (w * h) 0) VU.// (lst (read pos) css))
    where
      (w, h) = (fieldWidth fd, fieldHeight fd)
      lst (x, y) css = concatMap (f [x ..]) $ zip [y ..] css
      f is (j, cs) = [(posToIndex fd (i, j), conv c) | (i, c) <- zip is cs]
      conv c = if c == '#' then 1 else 0

-- | モデルを図形に変換する関数
drawModel :: Field -> Model -> Picture
drawModel fd cells = Pictures $ V.toList $ V.imap drawCell $ V.convert cells
  where drawCell i cell = if (cell == 1)
          then indexToDrawCell fd i cyan
          else Blank

-- | モデルを更新する関数
simCells :: Field -> ViewPort -> Float -> Model -> Model
simCells fd _ _ cells = VU.imap check cells
  where
    check i v
      | v == 1    = if (cellNum == 2 || cellNum == 3) then 1 else 0
      | otherwise = if (cellNum == 3)                 then 1 else 0
      where
        neighborLst = (neighborTbl fd) V.! i
        cellNum     = sum [cells VU.! i | i <- neighborLst]
