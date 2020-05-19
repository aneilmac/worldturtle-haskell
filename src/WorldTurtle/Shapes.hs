module WorldTurtle.Shapes
  ( turtleArrow
  ) where

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

-- | Creates the default turtle polygon arrow with a given outline color and fill
--   color
turtleArrow :: Color -- ^ Outline color
            -> Color -- ^ Fill color
            -> Picture -- ^ Arrow shape.
turtleArrow o f = rotate 90 
                $ translate (-4) (-2) 
                $ pictures [outline_ o, fill_ f]

outline_ :: Color -> Picture
outline_ c = color c $ lineLoop [(0,0), (4, 2), (8, 0), (4, 8)]

fill_ :: Color -> Picture
fill_ c = color c $ pictures 
                  [ polygon [(0, 0), (4, 2), (1, 2)] -- left tail
                  , polygon [(4, 2), (8, 0), (7, 2)] -- right tail
                  , polygon [(1, 2), (7, 2), (4, 8)] -- main triangle
                  ]
