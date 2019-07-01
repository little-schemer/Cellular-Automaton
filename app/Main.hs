module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
-- import Field

main = display window white pic
  where
    window = InWindow "test" (800, 600) (0, 0)
    pic = Pictures [ Color black $ Line [(-400,0),(400,0)] <> Line [(0,-300),(0,300)]
                   , Scale 0.25 0.25 $ Color red $ Text "This is a pen."
                   , Color blue $ Line [(-400, 30),(400, 30)]
                   ]
