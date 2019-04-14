module Main where


import qualified Data.Vector as Vec
import System.Random
import Control.Monad
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Field


type Vec   = Vec.Vector
type Cells = Vec Int


state  =  10 :: Int
width  = 200 :: Int
height = 150 :: Int
cells  =   5 :: Float


main :: IO ()
main = do
  cells <- replicateM (width * height) (randomRIO (0, state))
  simulate window black 15 (Vec.fromList cells) (draw field) (simCells field)
    where
      field = initField width height cells
      window = InWindow "Cyclic" (windowSize field) (0, 0)

colors :: [Color]
colors = [makeColorI r g 150 a | (r, g, a) <- zip3 rLst gLst aLst]
  where
    rLst = [0, (div 255 state) .. 255]
    gLst = [255, 255 - (div 255 state) .. 0]
    aLst = cycle [150, 200]

draw :: Field -> Cells -> Picture
draw field cells = Pictures $ Vec.toList $ Vec.imap drawCell cells
  where drawCell i cell = Color (colors !! cell) $ indexToDrawCell field i

simCells :: Field -> ViewPort -> Float -> Cells -> Cells
simCells field _ _ cells = Vec.imap check cells
  where
    check i c = if (elem (mod (c + 1) state) lst) then (mod (c + 1) state) else c
      where
        f (x, y) = cells Vec.! (posToIndex field (x, y))
        lst = [f (x, y) | (x, y) <- vonNeumannN field (indexToPos field i)]
