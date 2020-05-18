{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}
module WorldTurtle.Internal.Turtle
  ( TurtleData
  , defaultTurtle
  , drawTurtle
  , heading
  , position
  , representation
  , penDown
  , speed
  , WorldTurtle.Internal.Turtle.scale
  , penColor
  ) where

import Control.Lens
import Control.Lens.TH

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import qualified Graphics.Gloss.Data.Picture as G (scale)

import qualified WorldTurtle.Internal.Coords as P

data TurtleData = TurtleData
    { _heading :: Float
    , _position :: P.Point 
    , _representation :: Picture
    , _penDown :: Bool
    , _speed :: Float
    , _scale :: Float
    , _penColor :: Color
    }

$(makeLenses ''TurtleData)

defaultTurtle :: TurtleData
defaultTurtle = TurtleData
    { _heading = 90
    , _position = (0, 0)
    , _representation = defaultTurtlePolygon
    , _penDown = True
    , _speed = 30
    , _scale = 1
    , _penColor = black
    }

drawTurtle :: TurtleData -> Picture
drawTurtle t = let (x, y) = _position t
                   s = _scale t
                in translate x y 
                 $ rotate (90 - t ^. heading)
                 $ G.scale s s
                 $ (t ^. representation)

defaultTurtlePolygon :: Picture
defaultTurtlePolygon = translate (-4) (-2) $ pictures [outline black, fill blue]

outline :: Color -> Picture
outline c = color c $ lineLoop [(0,0), (4, 2), (8, 0), (4, 8)]

fill :: Color -> Picture
fill c = color c $ pictures 
                 [ polygon [(0, 0), (4, 2), (1, 2)] -- left tail
                 , polygon [(4, 2), (8, 0), (7, 2)] -- right tail
                 , polygon [(1, 2), (7, 2), (4, 8)] -- main triangle
                 ]
