-- Credit for this implementation goes to:
-- https://juliagraphics.github.io/Luxor.jl/stable/turtle/
module Main where

import Graphics.Gloss.Geometry.Angle (degToRad)
import Graphics.WorldTurtle

import HueShift

hilbert :: Int -> Float -> Float -> Turtle -> TurtleCommand ()
hilbert 0 _ _ _ = pure ()
hilbert level angle lengthStep t = do
  penColor t >>= \c -> 
    setPenColor (shiftHue (degToRad 0.1) c) t
  
  left angle t
  hilbert' (-angle) t

  forward lengthStep t
  left (-angle)  t
  hilbert' angle t

  forward lengthStep t
  hilbert' angle t

  left (-angle) t
  forward lengthStep t
  hilbert' (-angle) t

  left angle t
  
  where hilbert' a = hilbert (level - 1) a lengthStep
 
main :: IO ()
main = runTurtle $ do
  t <- makeTurtle' (-200, -200) east red 
  setSpeed 2000 t
  setVisible False t
  hilbert 6 90 6 t

