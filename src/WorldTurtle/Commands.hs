module WorldTurtle.Commands
  ( Turtle
  , TurtleCommand
  , forward
  , backward
  , left
  , right
  , makeTurtle
  ) where

import WorldTurtle.Internal.Turtle
import WorldTurtle.Internal.Commands
import qualified WorldTurtle.Internal.Coords as P

import Graphics.Gloss.Data.Picture

import Control.Monad (when, void)

backward :: Turtle -> Float -> TurtleCommand ()
backward turtle d = forward turtle (-d)

forward :: Turtle -> Float -> TurtleCommand ()
forward turtle d = do
    t <- turtleData turtle
    -- ^ Get origin point
    let speed = _speed t
    void $ animate' d speed $ \ q -> do
      let startP = _position t
      let heading = P.degToRad $ _heading t
      let vec = P.rotateV heading (d, 0)
      let endP = vec P.+ startP
      let midP = P.lerp q startP endP
      -- ^ Get new endpoint via percentage
      when (_penDown t) $ do -- don't draw if pen isn't in down state
        let lPic = line [startP, midP] 
        -- ^ Draw line from startPoint to midPoint.
        addPicture lPic
        -- ^ Add line to our pictures system
      let t' = t { _position = midP }
      updateTurtle turtle t'
      -- ^ Update the turtle to a new position

right :: Turtle -> Float -> TurtleCommand ()
right t r = right t (-r)

left :: Turtle -> Float -> TurtleCommand ()
left turtle r = do
    t <- turtleData turtle
    let r' = normalizeHeading r
    void $ animate' (r' * pi) (_speed t * 180) $ \q -> do
      let heading = _heading t
      let newHeading = normalizeHeading $ heading + q * r'
      -- ^ Get new heading via percentage
      let t' = t { _heading = newHeading }
      updateTurtle turtle t'
      -- ^ Update turtle with the new normalized heading

-- | Return a valid heading value between (0, 360).
normalizeHeading :: Float -> Float
normalizeHeading f
  | f < 0     = normalizeHeading (f + r)
  | f > r     = normalizeHeading (f - r)
  | otherwise = f
  where r = 360.0

animate' :: Float -> Float -> (Float -> TurtleCommand a) -> TurtleCommand a
animate' distance speed callback =
   let duration = distance / speed
       d' = if isNaN duration || isInfinite duration then 0 else duration
       -- ^ if speed is 0 we use this as a "no animation" command from 
       --   user-space.
     in animate d' callback

animate :: Float -> (Float -> TurtleCommand a) -> TurtleCommand a
animate duration callback = do
   timeRemaining <- simTime -- simulation time to go
   let availableTime = min timeRemaining duration
   -- ^ Amount of time we have to complete the animation before we need to exit.
   let timeQuotient = if availableTime == 0 then 1 else availableTime / duration
   -- ^ quotient of available time vs required time. Note that when the duration
   --   is 0 we say "don't do any animation"
   t <- callback timeQuotient 
   -- ^ Perform the calculation with the quotient for lerping
   decrementSimTime availableTime 
   -- ^ Test to see if this is the end of our animation and if so exit early
   return t