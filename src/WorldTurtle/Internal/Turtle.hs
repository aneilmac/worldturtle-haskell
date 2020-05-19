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
  , visible
  ) where

import Control.Lens
import Control.Lens.TH

import WorldTurtle.Shapes

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
    , _visible :: Bool
    }

$(makeLenses ''TurtleData)

defaultTurtle :: TurtleData
defaultTurtle = TurtleData
    { _heading = 90
    , _position = (0, 0)
    , _representation = turtleArrow black blue
    , _penDown = True
    , _speed = 100
    , _scale = 1
    , _penColor = black
    , _visible = True
    }

drawTurtle :: TurtleData -> Picture
drawTurtle t
  | t ^. visible  == False = blank
  | otherwise = let (x, y) = _position t
                    s = _scale t
                 in translate x y 
                  $ rotate (360 - t ^. heading)
                  $ G.scale s s
                  $ (t ^. representation)
