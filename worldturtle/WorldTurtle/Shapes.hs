module WorldTurtle.Shapes
  ( turtleArrow
  , thickLine
  ) where

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

import qualified WorldTurtle.Internal.Coords as P

-- | Creates the default turtle polygon arrow with a given outline color and fill
--   color
turtleArrow :: Color -- ^ Outline color
            -> Color -- ^ Fill color
            -> Picture -- ^ Arrow shape.
turtleArrow o f = rotate 90 $ pictures [outline_ o, fill_ f]

-- | Draws a line from point @A@ to point @B@ with a given thickness.
thickLine :: Point -- ^ Starting point @A@.
          -> Point  -- ^ Ending point @B@.
          -> Float -- ^ Line thickness.
          -> Picture -- ^ Produced line.
thickLine a b t = 
  let v = b P.- a
      lineLength = P.magV v
      angle = P.radToDeg $ P.argV v
   in translate (fst a) (snd a)
    $ rotate (360 - angle)
    $ translate (lineLength/2) 0
    $ rectangleSolid (lineLength + t) t

outline_ :: Color -> Picture
outline_ c = color c $ translate (0) (-0.5) $ scale 1.2 1.2 $ fill_ c

fill_ :: Color -> Picture
fill_ c = color c $ translate (-4) (-2) 
                  $ pictures 
                  [ polygon [(0, 0), (4, 2), (1, 2)] -- left tail
                  , polygon [(4, 2), (8, 0), (7, 2)] -- right tail
                  , polygon [(1, 2), (7, 2), (4, 8)] -- main triangle
                  ]
