module Main where

import Control.Monad (forever, guard) -- Required control flow functions.
import Graphics.Gloss.Data.Vector (magV) -- Grab the magnitude function.

import Graphics.WorldTurtle

main :: IO ()
main = runWorld $ makeTurtle' (0, 0) north red >>= flip run (forever drawStar)

drawStar :: TurtleCommand ()
drawStar = do
  -- Draw a star point.
  forward 200
  left 170
  -- If the magnitude of the position is <1 then stop animation.
  position >>= guard . (1 <=) . magV