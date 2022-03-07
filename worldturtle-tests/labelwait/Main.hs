module Main where

import Graphics.WorldTurtle
import Control.Monad.IO.Class

main :: IO ()
main = runWorld' orange $ do
    t1 <- makeTurtle
    t2 <- makeTurtle 
    t2 >/> jump (50, 0)
    liftIO $ putStrLn "Preparing to draw."
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