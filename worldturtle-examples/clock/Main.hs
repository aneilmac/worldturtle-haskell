{-| This more unusual example shows how to render an animated clock showing the
    current time in hours minutes and seconds.
-}
module Main where

import Control.Monad (liftM2, forM_, forever)

import Data.Time.Clock (getCurrentTime)

import Data.Time.LocalTime (TimeOfDay (..), getCurrentTimeZone, utcToLocalTime
                           , localTimeOfDay)

import Graphics.WorldTurtle

import qualified Graphics.Gloss.Data.Picture as G

main :: IO ()
main = do
  localTime <- localTimeOfDay <$> liftM2 utcToLocalTime 
                                         getCurrentTimeZone 
                                         getCurrentTime
  runWorld $ do
    drawFace
    s <- makeHand green  secsRadius  secsInMinute $ secsToDeg localTime
    m <- makeHand orange minsRadius  secsInHour   $ minsToDeg localTime
    h <- makeHand red    hoursRadius secsInDay    $ hoursToDeg localTime
    moveHand secsRadius s <|> moveHand minsRadius m <|> moveHand hoursRadius h

moveHand :: Float -> Turtle -> WorldCommand ()
moveHand radius = run $ forever $ circle (-radius)

makeHand :: Color -> Float -> Float -> Float -> WorldCommand Turtle
makeHand c radius time offset = do
  t <- makeTurtle' (0, radius) east c
  t >/> do
    setRepresentation $ G.color c $ G.pictures [ G.line [(0, 0), (0, -radius)]
                                               , G.circleSolid 10
                                               ]
    setPenDown False
    setSpeed 0 -- Instant draw
    arc (-radius) offset -- Catch the hand up to where it should be.
    setSpeed (pi * 2 * radius / time)
  return t

drawFace ::  WorldCommand ()
drawFace = do
  t <- makeTurtle' (0, -clockRadius) east black
  t >/> do
    setSpeed 0 -- Instant draw
    setRotationSpeed 0 -- Instant draw
    setVisible False
    setPenDown False
    forM_ ([0,5..355] :: [Int]) $ \time -> do
      branch $ do
        left 90
        setPenDown True
        let l = if time `mod` 15 == 0 then 20 else 10
        forward l
      arc clockRadius 5

clockRadius :: Float
clockRadius = 130

secsRadius :: Float
secsRadius = 120

minsRadius :: Float
minsRadius = 100

hoursRadius :: Float
hoursRadius = 70

secsInMinute :: Float
secsInMinute = 60

secsInHour :: Float
secsInHour = 3600

secsInDay :: Float
secsInDay = 86400

secsToDeg :: TimeOfDay -> Float
secsToDeg l = 360 * realToFrac (todSec l) / secsInMinute

minsToDeg :: TimeOfDay -> Float
minsToDeg l = 360 * secsInMinute * realToFrac (todMin l) / secsInHour

hoursToDeg :: TimeOfDay -> Float
hoursToDeg l = 720 * secsInHour * realToFrac (todHour l) / secsInDay
