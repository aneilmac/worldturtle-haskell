module Main where

import WorldTurtle

drawSquare1 :: TurtleCommand ()
drawSquare1 = do
  t <- makeTurtle
  --drawIt $ defaultTurtlePolygon
  forward t 90
  left t 90
  forward t 90
  left t 90
  forward t 90
  left t 90
  forward t 90

drawSquare2 :: TurtleCommand ()
drawSquare2 = do
  t <- makeTurtle
  left t 180
  --drawIt $ defaultTurtlePolygon
  forward t 90
  left t 90
  forward t 90
  left t 90
  forward t 90
  left t 90
  forward t 90

drawTest :: TurtleCommand ()
drawTest = drawSquare1 <> drawSquare2

main :: IO ()
main = runTurtle drawTest

