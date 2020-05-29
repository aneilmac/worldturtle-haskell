-- | As taken from: 
-- http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/2008/tutorials/Tutorial7.pdf
module Main where

import Graphics.WorldTurtle

tree :: Int -> Turtle -> TurtleCommand ()
tree x t = f x
  where f 0 = setPenColor red t >> fd 10 t
        f y = g y' >> branch (lt 45 t >> f y')
                   >> branch (rt 45 t >> f y')
                   >> branch (g y' >> f y')
              where y' = y - 1
        g 0 = setPenColor blue t >> fd 10 t
        g y = g y' >> g y'
              where y' = y - 1


main :: IO ()
main = runTurtle $ do
  t <- makeTurtle
  setVisible False t
  setRotationSpeed 0 t
  setSpeed 500 t
  tree 6 t
