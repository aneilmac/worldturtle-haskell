-- Credit for this implementation goes to:
-- https://juliagraphics.github.io/Luxor.jl/stable/turtle/
module Main where

import Graphics.Gloss.Geometry.Angle (degToRad)
import Graphics.WorldTurtle

hilbert :: Int -> Float -> Float -> TurtleCommand ()
hilbert 0 _ _ = pure ()
hilbert level angle lengthStep = do
  penColor >>= \c -> setPenColor $ shiftHue (degToRad 0.1) c

  left angle
  hilbert' (-angle)

  forward lengthStep
  left (-angle)
  hilbert' angle

  forward lengthStep
  hilbert' angle

  left (-angle)
  forward lengthStep
  hilbert' (-angle)

  left angle
  
  where hilbert' a = hilbert (level - 1) a lengthStep
 
main :: IO ()
main = runWorld $ do
  t <- makeTurtle' (-200, -200) east red 
  t >/> do
    setSpeed 2000
    setRotationSpeed 0
    setVisible False
    hilbert 6 90 6
