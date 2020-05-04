--
-- コマンドラインオプション
--   + -n : "n" = フォン・ノイマン近傍, "m" = ムーア近傍
--   + -s : 状態の数
--
--   ex : stack exec -- cyclic -s 10 -n n
--

module Main where


import           Control.Applicative
import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as VU
import           Field
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Options.Applicative
import           System.Random


type Model = (VU.Vector Int, Int, [Color])


---------------------------------------------------
-- * パラメータ
---------------------------------------------------

width  = 300 :: Int
height = 200 :: Int
size   =   3 :: Float
state  =  16 :: Int


---------------------------------------------------
-- * コマンドオプション
---------------------------------------------------

data Option = Option
  { fdWidth  :: Int
  , fdHeight :: Int
  , neighbor :: String
  , cState   :: Int
  }

opt :: Parser Option
opt = Option
  <$> option auto (short 'w' <> value width)
  <*> option auto (short 'h' <> value height)
  <*> strOption   (short 'n' <> value "n")
  <*> option auto (short 's' <> value state)


---------------------------------------------------
-- * Cyclic
---------------------------------------------------

main :: IO ()
main = do
  Option w h n st <- execParser (info opt mempty)
  let fd = initField w h size (nf n)
  cells <- VU.replicateM (w * h) (randomRIO (0, st))
  let model = (cells, st, colors st)
  let window = InWindow "Cyclic" (windowSize fd) (0, 0)
  simulate window black 15 model (drawModel fd) (simModel fd)
    where
      nf "n" = neumann
      nf "m" = moore

colors :: Int -> [Color]
colors st = [makeColorI r g 150 a | (r, g, a) <- zip3 rLst gLst aLst]
  where
    rLst = [0, (div 255 st) .. 255]
    gLst = [255, 255 - (div 255 st) .. 0]
    aLst = cycle [100, 200, 150, 250]

drawModel :: Field -> Model -> Picture
drawModel fd (cells, st, clrs) = Pictures $ V.toList $ V.imap drawCell $ V.convert cells
  where drawCell i cell = indexToDrawCell fd i (clrs !! cell)


simModel :: Field -> ViewPort -> Float -> Model -> Model
simModel fd _ _ (cells, st, clrs) = (VU.imap check cells, st, clrs)
  where
    check i c = if elem c' clrList then c' else c
      where
        c' = mod (c + 1) st
        posList = (neighborTbl fd) V.! i
        clrList = map (cells VU.!) posList
