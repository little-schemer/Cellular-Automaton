--
-- コマンドラインオプション
--   + -n : "n" = フォン・ノイマン近傍, "m" = ムーア近傍
--   + -s : 状態の数
--
--   ex : stack exec -- cyclic -s 10 -n n
--

module Main where


import           Control.Applicative
import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as VU
import           Field
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           System.Environment
import           System.Random


type Model = (VU.Vector Int, Int, [Color])


width  = 300 :: Int
height = 200 :: Int
size   =   4 :: Float
state  =  16 :: Int
field  = initField width height size neumann


main :: IO ()
main = do
  cells <- VU.replicateM (width * height) (randomRIO (0, state))
  simulate window black 15 (cells, state, colors state) drawModel simModel
    where window = InWindow "Cyclic" (windowSize width height size) (0, 0)


-- cmd :: [String] -> (Int, Int)
-- cmd args = loop args (state, fst)
--   where
--     loop [] opt                       = opt
--     loop ("-s" : s : args') (_, nb)   = loop args' (read s, nb)
--     loop ("-n" : "n" : args') (st, _) = loop args' (st, fst)
--     loop ("-n" : "m" : args') (st, _) = loop args' (st, snd)
--     loop (_ : args') (st, nb)         = loop args' (st, nb)


colors :: Int -> [Color]
colors st = [makeColorI r g 150 a | (r, g, a) <- zip3 rLst gLst aLst]
  where
    rLst = [0, (div 255 st) .. 255]
    gLst = [255, 255 - (div 255 st) .. 0]
    aLst = cycle [100, 200, 150, 250]


drawModel :: Model -> Picture
drawModel (cells, st, clrs) = Pictures $ V.toList $ V.imap drawCell $ VU.convert cells
  where drawCell i cell = indexToDrawCell field i (clrs !! cell)


simModel :: ViewPort -> Float -> Model -> Model
simModel _ _ (cells, st, clrs) = (VU.imap check cells, st, clrs)
  where
    check i c = if elem c' clrList then c' else c
      where
        c' = mod (c + 1) st
        posList = (neighborhoodTable field) V.! i
        clrList = map (cells VU.!) posList
