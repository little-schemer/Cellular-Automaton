module Main where

import           Control.Monad
import qualified Data.Vector                  as Vec
import           Field
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           System.Random


type Model = Vec.Vector Bool

width  = 400 :: Int
height = 300 :: Int
size   =   3 :: Float
field  = initField width height size


main :: IO ()
main = do
  cells <- Vec.replicateM (width * height) (randomIO :: IO Bool)
  simulate window black 15 cells drawModel simCells
    where window = InWindow "Life Game" (windowSize width height size) (0, 0)

drawModel :: Model -> Picture
drawModel cells = cellPic
  where
    cellPic = Pictures $ Vec.toList $ Vec.imap drawCell cells
    drawCell i cell = if cell
                      then Color cyan $ indexToDrawCell field i
                      else Blank

simCells :: ViewPort -> Float -> Model -> Model
simCells _ _ cells = Vec.imap check cells
  where
    check i cell
      | cell      = if (cellNum == 2 || cellNum == 3) then True else False
      | otherwise = if (cellNum == 3)                 then True else False
      where
        nbs i = let (a, b) = (neighborhoodTable field) Vec.! i in a ++ b
        cellNum = length $ filter (\x -> x) $ map (cells Vec.!) $ nbs i


-- cells = Vec.fromList [False,True,True,False,True,False,True,False,True,False,False,False,True,False,True,True,True,True,False,False,False,False,False,True,False,True,False,True,True,True,True,False,False,True,False,True,True,False,True,False,True,False,False,True,False,True,False,False,False,True,False,True,True,False,False,True,False,False,True,True,True,True,False,False,True,False,False,True,False,False,True,True,True,False,True,True,False,True,False,False,False,True,True,False,False,False,False,True,True,False,False,True,False,False,True,True,False,True,True,True,True,False,False,False,False,True,False,True,False,False,True,False,True,True,False,True,False,True,False,False,False,False,True,True,False,True,True,False,False,False,False,True,True,True,False,True,True,True,True,False,False,False,False,False,False,True,True,True,False,False,False,False,False,True,False,False,False,True,True,True,False,True,False,False,False,True,True,False,True,True,True,False,True,False,True,True,False,True,False,False,True,False,False,False,True,False,False,True,False,False,True,True,True,True,False,False,True,False,False,False,True,True,False,False,False,False,False,True,True,True,False,False,True,False,True,False,True,True,False,True,True,False,True,False,False,True,True,False,True,True,False,False,True,True,True,True,True,False,True,True,False,True,True,True,True,False,False,False,False,False,False,False,False,False,True,False,True,True,True,False,False,False,False,False,False,False,False,True,True,True,False,True,False,False,False,True,True,True,True,True,False,True,True,True,True,True,False,False,False,False,False,True,True,False,False,True,False,True,False,True]
