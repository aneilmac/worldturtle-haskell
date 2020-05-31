-- As taken from: 
-- http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/2008/tutorials/Tutorial7.pdf
module Main where

import Graphics.WorldTurtle

tree :: Int -> TurtleCommand ()
tree = f
  where f 0 = setPenColor red >> fd 10
        f y = g y' >> branch (lt 45 >> f y')
                   >> branch (rt 45 >> f y')
                   >> branch (g y' >> f y')
              where y' = y - 1
        g 0 = setPenColor blue >> fd 10
        g y = g y' >> g y'
              where y' = y - 1

main :: IO ()
main = runTurtle $ do
  setVisible False
  setRotationSpeed 0
  setSpeed 500
  tree 6
