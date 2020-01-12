module Main where

import           Control.Monad
import           Data.Maybe
import qualified Data.Vector                  as V
import qualified Data.Vector.Mutable          as M
import qualified Data.Vector.Unboxed          as VU
import           Field
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           System.Random


type State = Int
type Motion = Int

data Model = Model { cells   :: VU.Vector Color
                   , antHead :: Int
                   , antPos  :: Position
                   , antSt   :: Int
                   , rule    :: [((Color, State), (Color, Motion, State))]
                   } -- deriving Show

width  = 300 :: Int
height = 200 :: Int
size   =   4 :: Float
field  = initField width height size

toRight  =  1 :: Int
toLeft   = -1 :: Int
toFoward =  0 :: Int

--
-- 1 サイクル
--  + アリのいる Cell の色を読む。
--  + アリの状態と記号の組み合わせに従って表を引く。
--  + 表の指示に従って新しい色を Cell に書き込み、向きを変え、一歩進む。
--

main :: IO ()
main = simulate window black 15 model drawModel simAnt
  where
    window = InWindow "Langton's Ant" (windowSize field) (0, 0)
    model  = Model { cells   = VU.replicate (width * height) black
                   , antHead = 0
                   , antPos  = (div width 2, div height 2)
                   , antSt   = 0
                   , rule    = [((black, 0), (red,   toLeft,  0))
                               ,((red  , 0), (black, toRight, 0))] }

drawModel :: Model -> Picture
drawModel md = Pictures [cellPic, ant]
  where
    cellPic = Pictures $ V.toList $ V.imap drawCell $ VU.convert (cells md)
    ant = indexToDrawCell field (posToIndex window $ antPos model) cyan
    drawCell i cell = if (cell /= black)
                      then indexToDrawCell fd i cell
                      else Blank

simAnt :: ViewPort -> Float -> Model -> Model
simAnt _ _ md = md { cells = cells', antHead = antHead', antPos = antPos' }
  where
    antIdx = posToIndex field $ antPos md
    (color, move, state) = fromJust $ lookup (cellColor, antSt md) (rule md)
      where cellColor = (cells md) V.! antIdx
    cells' = VU.modify (\v -> M.write v antIdx color) (cells md)
    antHead' = mod (antHead md + move) 4
    antPos' = (vonNeumannN fd $ antPos md) !! antHead'
