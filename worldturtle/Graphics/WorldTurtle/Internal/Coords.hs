{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
module Graphics.WorldTurtle.Internal.Coords
  ( module GPoint
  , module GArithmetic
  , module GVector
  , module GAngle
  , lerp
  , normalizeHeading
  , normalizeDirection
  ) where

import Prelude hiding ((-), (+))
import qualified Prelude as P

import Graphics.Gloss.Data.Point as GPoint
import Graphics.Gloss.Data.Point.Arithmetic as GArithmetic
import Graphics.Gloss.Data.Vector as GVector
import Graphics.Gloss.Geometry.Angle as GAngle

lerp :: Float -> Point -> Point -> Point
lerp !l !a !b =  ((1 P.- l) `mulSV` a) + (l `mulSV` b)

-- | Return a valid heading value between (0, 360).
normalizeHeading :: Float -> Float
normalizeHeading !f
  | f < 0     = normalizeHeading (f P.+ r)
  | f > r     = normalizeHeading (f P.- r)
  | otherwise = f
  where r = 360.0

-- | Return a valid heading value between (-180, 180).
normalizeDirection :: Float -> Float
normalizeDirection !f
  | f < -r     = normalizeHeading (f P.+ r)
  | f >  r     = normalizeHeading (f P.- r)
  | otherwise = f
  where r = 180.0