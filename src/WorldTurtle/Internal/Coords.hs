module WorldTurtle.Internal.Coords
  ( module GPoint
  , module GArithmetic
  , module GVector
  , module GAngle
  , lerp
  ) where

import Prelude hiding ((-), (+))
import qualified Prelude as P

import Graphics.Gloss.Data.Point as GPoint
import Graphics.Gloss.Data.Point.Arithmetic as GArithmetic
import Graphics.Gloss.Data.Vector as GVector
import Graphics.Gloss.Geometry.Angle as GAngle

lerp :: Float -> Point -> Point -> Point
lerp l a b =  ((1 P.- l) `mulSV` a) + (l `mulSV` b)
