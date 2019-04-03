module Main where


import qualified Data.Vector as Vec
import System.Random
import Control.Monad
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Field


type Vec   = Vec.Vector
type Cells = Vec Int


colors :: [Color]
colors = [makeColorI r g 255 a | (r, g, a) <- zip3 rLst gLst aLst]
  where
    rLst = [0, (div 255 state) .. 255]
    gLst = [255, 255 - (div 255 state) .. 0]
    aLst = cycle [150, 200]

state :: Int
state = 10
main :: IO ()
main = do
  cells <- replicateM (width field * height field) (randomRIO (0, state))
  simulate window black 15 (Vec.fromList cells) (draw field) (simCells field)
    where
      field = Field { width = 200, height = 150, cellSize = 5 }
      sizeX = width field * (round (cellSize field))
      sizeY = height field * (round (cellSize field))
      window = InWindow "Cyclic" (sizeX, sizeY) (10, 10)

draw :: Field -> Cells -> Picture
draw field cells = Pictures $ Vec.toList $ Vec.imap drawCell cells
  where
    drawCell i cell =
      Translate x y $ Color (colors !! cell) $ rectangleSolid size size
      where
        (x, y) = indexToGlossPoint field i
        size = cellSize field - 1

simCells :: Field -> ViewPort -> Float -> Cells -> Cells
simCells field _ _ cells = Vec.imap check cells
  where
    check i c = if (elem (mod (c + 1) state) lst) then (mod (c + 1) state) else c
      where
        f (x, y) = cells Vec.! (posToIndex field (x, y))
        lst = [f (x, y) | (x, y) <- vonNeumannN field (indexToPos field i)]
