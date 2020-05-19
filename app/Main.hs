module Main where

import WorldTurtle
import qualified Graphics.Gloss.Data.Picture as G
import Control.Monad

m :: Float -> TurtleCommand ()
m d = do 
  t <- makeTurtle
  setSpeed t 130
  right t $ 0 + d
  circle t 90 360
      
n :: Float -> TurtleCommand() 
n d = do 
  t <- makeTurtle
  setSpeed t 130
  setPenColor t red
  right t $ 90 + d
  circle t 90 360

q :: Float -> TurtleCommand() 
q d = do 
  t <- makeTurtle
  setSpeed t 130
  setPenColor t green
  right t $ 180 + d
  circle t 90 360

r :: Float -> TurtleCommand() 
r d = do 
  t <- makeTurtle
  setSpeed t 130
  setPenColor t blue
  right t $ 270 + d
  circle t 90 360

loop :: Turtle -> Float -> TurtleCommand() 
loop t d = do 
  setSpeed t 500
  right t d
  circle t 90 360

main :: IO ()
main = runTurtle $ do
  t1 <- makeTurtle
  setPenColor t1 blue
  setHeading t1 90

  t2 <- makeTurtle
  setPenColor t2 red
  setHeading t2 180

  t3 <- makeTurtle
  setPenColor t3 green
  setHeading t3 270

  t4 <- makeTurtle
  setHeading t4 0

  forM_ [0, 5 ..] $ 
    \ v -> loop t1 v <> loop t2 v <> loop t3 v <> loop t4 v
