module Main where

import Graphics.WorldTurtle

drawTriangle :: TurtleCommand ()
drawTriangle = do
  setHeading east
  forward 100
  left 120
  forward 100
  left 120
  forward 100

main :: IO ()
main = runTurtle drawTriangle