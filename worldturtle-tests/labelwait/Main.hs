module Main where

import Graphics.WorldTurtle

main :: IO ()
main = runWorld' orange $ do
    t1 <- makeTurtle
    t2 <- makeTurtle 
    t2 >/> jump (50, 0)
    t1 >/> turtleF1 >!> t2 >/> turtleF2

turtleF1 :: TurtleCommand ()
turtleF1 = do
    wait 2
    fd 50 
    label "T1"

turtleF2 :: TurtleCommand ()
turtleF2 = do
    fd 50 
    label "T2"