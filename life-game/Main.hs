module Main where

import           Control.Monad
import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as VU
import           Field
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           System.Random


type Model = V.Vector Bool


width  = 500 :: Int
height = 250 :: Int
size   =   3 :: Float
field  = initField width height size


main :: IO ()
main = do
  cells <- V.replicateM (width * height) (randomIO :: IO Bool)
  simulate window black 15 cells drawModel simCells
    where window = InWindow "Life Game" (windowSize width height size) (0, 0)

drawModel :: Model -> Picture
drawModel cells = Pictures $ V.toList $ V.imap drawCell cells
  where
    drawCell i cell = if cell
                      then Color cyan $ indexToDrawCell field i
                      else Blank

simCells :: ViewPort -> Float -> Model -> Model
simCells _ _ cells = V.imap check cells
  where
    check i cell
      | cell      = if (cellNum == 2 || cellNum == 3) then True else False
      | otherwise = if (cellNum == 3)                 then True else False
      where
        nbs i = snd $ (neighborhoodTable field) V.! i
        cellNum = length $ filter id $ map (cells V.!) $ nbs i


-- cells = Vec.fromList [False,True,True,False,True,False,True,False,True,False,False,False,True,False,True,True,True,True,False,False,False,False,False,True,False,True,False,True,True,True,True,False,False,True,False,True,True,False,True,False,True,False,False,True,False,True,False,False,False,True,False,True,True,False,False,True,False,False,True,True,True,True,False,False,True,False,False,True,False,False,True,True,True,False,True,True,False,True,False,False,False,True,True,False,False,False,False,True,True,False,False,True,False,False,True,True,False,True,True,True,True,False,False,False,False,True,False,True,False,False,True,False,True,True,False,True,False,True,False,False,False,False,True,True,False,True,True,False,False,False,False,True,True,True,False,True,True,True,True,False,False,False,False,False,False,True,True,True,False,False,False,False,False,True,False,False,False,True,True,True,False,True,False,False,False,True,True,False,True,True,True,False,True,False,True,True,False,True,False,False,True,False,False,False,True,False,False,True,False,False,True,True,True,True,False,False,True,False,False,False,True,True,False,False,False,False,False,True,True,True,False,False,True,False,True,False,True,True,False,True,True,False,True,False,False,True,True,False,True,True,False,False,True,True,True,True,True,False,True,True,False,True,True,True,True,False,False,False,False,False,False,False,False,False,True,False,True,True,True,False,False,False,False,False,False,False,False,True,True,True,False,True,False,False,False,True,True,True,True,True,False,True,True,True,True,True,False,False,False,False,False,True,True,False,False,True,False,True,False,True]
