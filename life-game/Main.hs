module Main where

import qualified Data.Vector as Vec
import System.Random
import Control.Monad
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Field


type Vec   = Vec.Vector
type Cells = Vec Bool


main :: IO ()
main = do
  cells <- replicateM (width field * height field) randomIO
  -- let cells = setCell field [(10,10), (11,10),(12,10),(10,11),(11,12)] -- グライダー
  simulate window black 10 (Vec.fromList cells) (draw field) (simCells field)
    where
      field = Field { width = 200, height = 150, cellSize = 5}
      sizeX = width field * (truncate (cellSize field))
      sizeY = height field * (truncate (cellSize field))
      window = InWindow "Life Game" (sizeX, sizeY) (10, 10)

draw :: Field -> Cells -> Picture
draw field cells = Pictures $ Vec.toList $ Vec.imap drawCell cells
  where
    drawCell i cell = if cell
                      then Translate x y $ Color cyan $ rectangleSolid size size
                      else Blank
      where
        (x, y) = indexToGlossPoint field i
        size = cellSize field - 1

simCells :: Field -> ViewPort -> Float -> Cells -> Cells
simCells field _ _ cells = Vec.imap check cells
  where
    check i cell
      | cell      = if (count == 2 || count == 3) then True else False
      | otherwise = if (count == 3)               then True else False
      where
        f (x, y) = cells Vec.! (posToIndex field (x, y))
        bs = [f (x, y) | (x, y) <- mooreN field (indexToPos field i)]
        count = length $ filter (\x -> x) bs

setDataAt :: [a] -> [(Int, a)] -> [a]
setDataAt xs as = foldl f xs as
  where f xs (i, d) = let (as, _ : bs) = splitAt i xs in as ++ (d : bs)

setCell :: Field -> [Position] -> [Bool]
setCell field xs = setDataAt cells $ map f xs
  where
    cells = replicate (width field * height field) False
    f (x, y) = (posToIndex field (x, y), True)