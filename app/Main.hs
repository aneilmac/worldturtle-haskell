module Main where

import WorldTurtle

drawTest4 :: TurtleCommand ()
drawTest4 = do
  t <- makeTurtle
  --drawIt $ defaultTurtlePolygon
  forward t 90
  left t 90
  forward t 90
  left t 90
  forward t 90
  left t 90
  forward t 90

main :: IO ()
main = runTurtle drawTest4

