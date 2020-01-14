module Main where


import           Control.Applicative
import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as VU
import           Field
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           System.Environment
import           System.Random


-----------------------------------------
-- * パラメータ
-----------------------------------------

width  = 500 :: Int             -- 横の Cell 数
height = 250 :: Int             -- 縦の Cell 数
size   =   3 :: Float           -- Cell のサイズ
field  = initField width height size moore


type Model = [Int]

main :: IO ()
main = do
  let model = initModel
  simulate window black 15 model drawModel simModel
    where window = InWindow "test" (windowSize width height size) (0, 0)

initModel = undefined

drawModel :: Model -> Picture
drawModel = undefined

simModel :: ViewPort -> Float -> Model -> Model
simModel = undefined
