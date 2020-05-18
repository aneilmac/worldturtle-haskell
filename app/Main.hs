module Main where

import WorldTurtle

drawSquare1 :: TurtleCommand ()
drawSquare1 = do
  t <- makeTurtle
  --drawIt $ defaultTurtlePolygon
  forward t 90
  left t 90
  stamp t
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


m :: TurtleCommand ()
m = do 
  t <- makeTurtle
  forward t 90
  circle t 90 360
      
n :: TurtleCommand() 
n = do 
  t <- makeTurtle
  setPenColor t yellow
  stamp t
  right t 90
  forward t 90
  stamp t
  circle t 90 360

q :: TurtleCommand ()
q = do 
  t <- makeTurtle
  stamp t
  right t 180
  forward t 90
  stamp t
  circle t 90 360

r :: TurtleCommand ()
r = do 
  t <- makeTurtle
  stamp t
  left t 90
  forward t 90
  stamp t
  circle t 90 360

s :: TurtleCommand ()
s = do 
  t <- makeTurtle
  stamp t
  left t 45
  forward t 90
  stamp t
  circle t 90 360

t :: TurtleCommand ()
t = do 
  t <- makeTurtle
  stamp t
  setPenDown t False
  right t 45
  forward t 90
  stamp t
  circle t 90 360

moveTurtle1 :: TurtleCommand ()
moveTurtle1 = do
  t1 <- makeTurtle
  forward t1 90

moveTurtle2 :: TurtleCommand ()
moveTurtle2 = do
  t2 <- makeTurtle
  right t2 90
  forward t2 360

moveTurtle3 :: Float -> TurtleCommand ()
moveTurtle3 f = do
  t1 <- makeTurtle
  left t1 180
  forward t1 f
  left t1 90
  forward t1 90


main :: IO ()
main = runTurtle $ do
   n <> m <> q <> r <> s <> t
