module Main where

import Control.Monad (forever, guard) -- Required control flow functions.
import Graphics.Gloss.Data.Vector (magV) -- Grab the magnitude function.

import Graphics.WorldTurtle

main :: IO ()
main = runTurtle $ makeTurtle' (0, 0) north red >>= forever . drawStar
  where drawStar t = do
          -- Draw a star point.
          forward 200 t
          left 170 t
          -- If the magnitude of the position is <1 then stop animation.
          position t >>= guard . (1 <=) . magV