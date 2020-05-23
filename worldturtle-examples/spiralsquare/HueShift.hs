module HueShift 
  ( shiftHue
  ) where

import Data.Matrix

import Graphics.Gloss.Data.Color

shiftHue :: Float -> Color -> Color
shiftHue d c = let hMatrix = hueMatrix d
                   (r, g, b, a) = rgbaOfColor c
                   cMatrix = fromList 1 3 [r, g, b]
                   cMatrix' = cMatrix * hMatrix
                   [r', g', b'] = toList cMatrix'
                   in makeColor r' g' b' a

-- Haskell form of solution posted here:
-- https://stackoverflow.com/questions/8507885/shift-hue-of-an-rgb-color
hueMatrix :: Float -> Matrix Float
hueMatrix degrees = matrix 3 3 (calcForIndex degrees)

calcForIndex :: Float -> (Int, Int) -> Float
calcForIndex d (1, 1) = diag_ d
calcForIndex d (1, 2) = perm1_ d
calcForIndex d (1, 3) = perm2_ d 
calcForIndex d (2, 1) = perm2_ d 
calcForIndex d (2, 2) = diag_ d
calcForIndex d (2, 3) = perm1_ d
calcForIndex d (3, 1) = perm1_ d
calcForIndex d (3, 2) = perm2_ d 
calcForIndex d (3, 3) = diag_ d
calcForIndex _ _ = error $ "We only work with 3x3 matrices!"

diag_ :: Float -> Float
diag_ d = cos d + 1/3 * (1.0 - cos d)

perm1_ :: Float -> Float
perm1_ d = 1/3 * (1 - cos d) - sqrt (1/3) * sin d

perm2_ :: Float -> Float
perm2_ d = 1/3 * (1 - cos d) + sqrt (1/3) * sin d