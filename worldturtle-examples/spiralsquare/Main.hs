-- Credit for this implementation goes to:
-- https://juliagraphics.github.io/Luxor.jl/stable/turtle/
module Main where

import Control.Monad (forM_)

import Graphics.Gloss.Geometry.Angle (degToRad)
import Graphics.WorldTurtle

import HueShift

forwards :: [Float]
forwards = take 400 $ map (+1) [0, 0.75..]

spiral :: Turtle -> TurtleCommand ()
spiral t = do
  setPenColor cyan t
  setPenSize 1.5 t
  setSpeed 500 t
  forM_ forwards $ \ f -> do
    forward f t
    left 89.5 t
    penColor t >>= \c -> setPenColor (shiftHue (degToRad 1) c) t

main :: IO ()
main = runTurtle $ makeTurtle >>= spiral

