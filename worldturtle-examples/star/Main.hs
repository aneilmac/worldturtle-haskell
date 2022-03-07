module Main where

import Control.Monad (when)
import Graphics.Gloss.Data.Vector (magV) -- Grab the magnitude function.

import Graphics.WorldTurtle

main :: IO ()
main = runWorld $ makeTurtle' (0, 0) north red >>= run drawStar

drawStar :: TurtleCommand ()
drawStar = do
  -- Draw a star point.
  forward 200
  left 170
  -- If the magnitude of the position is <= 1 then return True.
  res <- (1 <= ) . magV <$> position
  when res drawStar