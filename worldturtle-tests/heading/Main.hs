module Main where

import Graphics.WorldTurtle
import qualified Graphics.Gloss.Data.Picture as G
import Control.Monad (forever)

main :: IO ()
main = runWorld $ do
    arm1 <- armTurtle 180
    arm2 <- armTurtle 90

    arm1 >/> do
        goto (50, 50)
        goto (0, 50)
        goto (0, 0)
    forever (arm1 >/> rt 360) >!> forever (arm2 >/> rt 360)


armTurtle :: Float -> WorldCommand Turtle
armTurtle rSpeed = do
    t <- makeTurtle
    t >/> do
        setRepresentation $ G.color black $ G.line [(0, 0), (50, 0)]
        setRotationSpeed rSpeed
    return t