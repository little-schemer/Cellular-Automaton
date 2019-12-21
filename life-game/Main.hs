module Main where

import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as VU
import           Field
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           System.Random


type Model = VU.Vector Bool


width  = 500 :: Int
height = 250 :: Int
size   =   3 :: Float
field  = initField width height size


main :: IO ()
main = do
  cells <- VU.replicateM (width * height) (randomIO :: IO Bool)
  simulate window black 15 cells drawModel simCells
    where window = InWindow "Life Game" (windowSize width height size) (0, 0)

drawModel :: Model -> Picture
drawModel cells = Pictures $ V.toList $ V.imap drawCell $ VU.convert cells
  where
    drawCell i cell = if cell
                      then Color cyan $ indexToDrawCell field i
                      else Blank

simCells :: ViewPort -> Float -> Model -> Model
simCells _ _ cells = VU.imap check cells
  where
    check i bool
      | bool      = if (cellNum == 2 || cellNum == 3) then True else False
      | otherwise = if (cellNum == 3)                 then True else False
      where
        nbs i = snd $ (neighborhoodTable field) V.! i
        cellNum = length $ filter id $ map (cells VU.!) $ nbs i
