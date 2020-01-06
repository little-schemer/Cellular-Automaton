module Main where


import           Control.Applicative
import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as VU
import           Field
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           System.Environment
import           System.Random


type Model = VU.Vector Bool


width  = 500 :: Int
height = 250 :: Int
size   =   3 :: Float
field  = initField width height size


main :: IO ()
main = do
  args <- getArgs
  cells <- if null args
           then VU.replicateM (width * height) (randomIO :: IO Bool)
           else initCells $ head args
  simulate window black 15 cells drawModel simCells
    where window = InWindow "Life Game" (windowSize width height size) (0, 0)

initCells :: FilePath -> IO Model
initCells path = do
  (pos : text) <- lines <$> readFile path
  return ((VU.replicate (width * height) False) VU.// (g (read pos) text))
    where
      g (x, y) css = concatMap (h [x ..]) $ zip [y ..] css
      h is (j, cs) = [(posToIndex width (i, j), conv c) | (i, c) <- zip is cs]
      conv c = if c == '#' then True else False


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
        (_, m) = (neighborhoodTable field) V.! i
        cellNum = length $ filter id $ map (cells VU.!) m
