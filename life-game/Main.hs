module Main where

import qualified Data.Vector as Vec
import System.Random
import Control.Monad
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Field


type Vec   = Vec.Vector
data Model = Model { cells :: Vec Bool, count :: Int }


width    = 200 :: Int
height   = 150 :: Int
cellSize =   5 :: Float


main :: IO ()
main = do
  cs <- randomCells
  -- let cells = setCell field [(10,10), (11,10),(12,10),(10,11),(11,12)] -- グライダー
  let model = Model { cells = Vec.fromList cs, count = 0 }
  simulate window black 20 model (drawModel field) (simCells field)
    where
      field  = initField width height cellSize
      window = InWindow "Life Game" (windowSize field) (0, 0)

randomCells :: IO [Bool]
randomCells = do
  cs <- replicateM (width * height) randomIO
  return cs

drawModel :: Field -> Model -> Picture
drawModel fd cs = Pictures [cellPic, msgPic]
  where
    cellPic = Pictures $ Vec.toList $ Vec.imap drawCell (cells cs)
    msgPic  = dispMsg fd white ("Step : " ++ show (count cs))
    drawCell i cell = if cell
                      then Color cyan $ indexToDrawCell fd i
                      else Blank

simCells :: Field -> ViewPort -> Float -> Model -> Model
simCells field _ _ cs = cs'
  where
    cs' = cs { cells = Vec.imap check (cells cs), count = count cs + 1 }
    check i cell
      | cell      = if (cellNum == 2 || cellNum == 3) then True else False
      | otherwise = if (cellNum == 3)                 then True else False
      where
        f (x, y) = (cells cs) Vec.! (posToIndex field (x, y))
        bs = [f (x, y) | (x, y) <- mooreN field (indexToPos field i)]
        cellNum = length $ filter (\x -> x) bs

-- setDataAt :: [a] -> [(Int, a)] -> [a]
-- setDataAt xs as = foldl f xs as
--   where f xs (i, d) = let (as, _ : bs) = splitAt i xs in as ++ (d : bs)

-- setCell :: Field -> [Position] -> [Bool]
-- setCell field xs = setDataAt cells $ map f xs
--   where
--     cells = replicate (width field * height field) False
--     f (x, y) = (posToIndex field (x, y), True)
