{-|
Module      : Graphics.WorldTurtle.Color
Description : Color functions
Copyright   : (c) Archibald Neil MacDonald, 2020
License     : BSD3
Maintainer  : FortOyer@hotmail.co.uk
Stability   : experimental
Portability : POSIX

This module is a collection of color manipulation commands!

-}
module Graphics.WorldTurtle.Color
  ( module Graphics.Gloss.Data.Color
  , shiftHue
  ) where

import Data.Matrix

import Graphics.Gloss.Data.Color

import Graphics.WorldTurtle.Internal.Coords (degToRad)

-- | Rotates a given color's hue between [0, 360) degrees.
shiftHue :: Float -- ^ Degrees to change hue.
         -> Color -- ^ Color to shift.
         -> Color -- ^ Resultant color with hue shifted.
shiftHue d c = let d' = degToRad d -- Radians to degrees
                   hMatrix = hueMatrix d'
                   (r, g, b, a) = rgbaOfColor c
                   cMatrix = fromList 1 3 [r, g, b]
                   cMatrix' = cMatrix * hMatrix
                   [r', g', b'] = toList cMatrix'
                   in makeColor r' g' b' a

-- Haskell form of solution posted here:
-- https://stackoverflow.com/questions/8507885/shift-hue-of-an-rgb-color
hueMatrix :: Float -> Matrix Float
hueMatrix degrees = matrix 3 3 (`calcForIndex` degrees)

calcForIndex :: (Int, Int) -> Float -> Float
calcForIndex (1, 1) = diag_
calcForIndex (1, 2) = perm1_
calcForIndex (1, 3) = perm2_ 
calcForIndex (2, 1) = perm2_ 
calcForIndex (2, 2) = diag_
calcForIndex (2, 3) = perm1_
calcForIndex (3, 1) = perm1_
calcForIndex (3, 2) = perm2_ 
calcForIndex (3, 3) = diag_
calcForIndex _      = error "We only work with 3x3 matrices!"

diag_ :: Float -> Float
diag_ d = cos d + 1/3 * (1.0 - cos d)

perm1_ :: Float -> Float
perm1_ d = 1/3 * (1 - cos d) - sqrt (1/3) * sin d

perm2_ :: Float -> Float
perm2_ d = 1/3 * (1 - cos d) + sqrt (1/3) * sin d
