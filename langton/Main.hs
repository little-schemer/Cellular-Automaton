module Main where

import           Control.Monad
import           Data.Maybe
import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as VU
import           Field
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort


type State  = Int
type Motion = Int

data Model = Model { cells   :: V.Vector Color
                   , antHead :: Int
                   , antPos  :: Position
                   , antSt   :: State
                   , rule    :: [((Color, State), (Color, Motion, State))]
                   } -- deriving Show

width  = 300 :: Int
height = 200 :: Int
size   =   4 :: Float
field  = initField width height size neumann

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
main = simulate window black 30 model drawModel simAnt
  where
    window = InWindow "Langton's Ant" (windowSize field) (0, 0)
    model  = Model { cells   = V.replicate (width * height) black
                   , antHead = 1
                   , antPos  = (div width 2, div height 2)
                   , antSt   = 0
                   , rule    = [((black, 0), (red,   toLeft,  0))
                               ,((red  , 0), (black, toRight, 0))] }

drawModel :: Model -> Picture
drawModel md = Pictures [cellPic, ant]
  where
    cellPic = Pictures $ V.toList $ V.imap drawCell (cells md)
    ant = indexToDrawCell field (posToIndex field $ antPos md) cyan
    drawCell i cell = if (cell /= black)
                      then indexToDrawCell field i cell
                      else Blank

simAnt :: ViewPort -> Float -> Model -> Model
simAnt _ _ md = md { cells = cells', antHead = antHead', antPos = antPos' }
  where
    antIdx = posToIndex field $ antPos md
    (clr, move, state) = fromJust $ lookup (cellColor, antSt md) (rule md)
      where cellColor = (cells md) V.! antIdx
    cells' = (cells md) V.// [(antIdx, clr)]
    antHead' = mod (antHead md + move) 4
    antPos' = indexToPos field $ ((neighborTbl field) V.! antIdx) !! antHead'
