{-# LANGUAGE BangPatterns #-}
{-|
Module      : Graphics.WorldTurtle.Shapes
Description : WorldTurtle
Copyright   : (c) Archibald Neil MacDonald, 2020
License     : BSD3
Maintainer  : FortOyer@hotmail.co.uk
Stability   : experimental
Portability : POSIX

This module exposes shapes not found in gloss but may be found to be worthwhile.

-}
module Graphics.WorldTurtle.Shapes
  ( turtleArrow
  , thickLine
  ) where

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

import qualified Graphics.WorldTurtle.Internal.Coords as P

-- | Creates the default turtle polygon arrow with a given outline color and 
--   fill color.
turtleArrow :: Color -- ^ Outline color
            -> Color -- ^ Fill color
            -> Picture -- ^ Arrow shape.
turtleArrow !o !f = rotate 90 $! pictures [outline_ o, fill_ f]

-- | Draws a line from a start-point to an end-point with a given thickness.
thickLine :: Point -- ^ Starting point.
          -> Point  -- ^ Ending point.
          -> Float -- ^ Line thickness.
          -> Picture -- ^ Produced line.
thickLine a b t = polygon [a1, a2, b2, b1]
  where !v = b P.- a
        !angle = P.argV v
        !perpAngle = angle - (pi/2)
        !t2 = t / 2
        !t' = P.rotateV angle (t2, 0)
        !t'' = P.rotateV perpAngle (t2, 0)
        !a1 = a P.- t'' P.- t'
        !a2 = a P.+ t'' P.- t'
        !b1 = b P.- t'' P.+ t'
        !b2 = b P.+ t'' P.+ t'

outline_ :: Color -> Picture
outline_ !c = color c $ translate 0 (-1) $ scale 1.4 1.4 $ fill_ c

fill_ :: Color -> Picture
fill_ !c = color c $ translate (-4) (-2) 
                  $ pictures 
                  [ polygon [(0, 0), (4, 2), (1, 2)] -- left tail
                  , polygon [(4, 2), (8, 0), (7, 2)] -- right tail
                  , polygon [(1, 2), (7, 2), (4, 8)] -- main triangle
                  ]
