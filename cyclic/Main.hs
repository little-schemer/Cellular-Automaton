--
-- コマンドラインオプション
--   + -n : "n" = フォン・ノイマン近傍, "m" = ムーア近傍
--   + -s : 状態の数
--
--   ex : stack exec -- cyclic -s 10 -n n
--

module Main where


import qualified Data.Vector as Vec
import System.Random
import Control.Monad
import System.Environment
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
  args <- getArgs
  let (st, nb) = cmd args
  cells <- replicateM (width * height) (randomRIO (0, st))
  simulate window black 20
    (Vec.fromList cells) (draw field (colors st)) (simCells field st nb)
    where
      field = initField width height cells
      window = InWindow "Cyclic" (windowSize field) (0, 0)

cmd :: [String] -> (Int, Field -> Position -> [Position])
cmd args = loop args (state, vonNeumannN)
  where
    loop [] opt = opt
    loop ("-s" : s : args') (_ , nb) = loop args' (read s, nb)
    loop ("-n" : "n" : args') (st, _) = loop args' (st, vonNeumannN)
    loop ("-n" : "m" : args') (st, _) = loop args' (st, mooreN)
    loop (_ : _ : args') (st, nb) = loop args' (st, nb)

colors :: Int -> [Color]
colors st = [makeColorI r g 150 a | (r, g, a) <- zip3 rLst gLst aLst]
  where
    rLst = [0, (div 255 st) .. 255]
    gLst = [255, 255 - (div 255 st) .. 0]
    aLst = cycle [100, 200, 150, 250]

draw :: Field -> [Color] -> Cells -> Picture
draw field clrs cells = Pictures $ Vec.toList $ Vec.imap drawCell cells
  where drawCell i cell = Color (clrs !! cell) $ indexToDrawCell field i

simCells :: Field -> Int -> (Field -> Position -> [Position])
         -> ViewPort -> Float -> Cells -> Cells
simCells field st nb _ _ cells = Vec.imap check cells
  where
    check i c = if (elem (mod (c + 1) st) lst) then (mod (c + 1) st) else c
      where
        f (x, y) = cells Vec.! (posToIndex field (x, y))
        lst = [f (x, y) | (x, y) <- nb field (indexToPos field i)]
