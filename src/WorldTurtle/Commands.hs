module WorldTurtle.Commands
  ( Turtle
  , TurtleCommand
  , forward
  , backward
  , left
  , right
  , makeTurtle
  , runTurtle
  ) where

import WorldTurtle.Internal.Turtle
import WorldTurtle.Internal.Commands
import qualified WorldTurtle.Internal.Coords as P

import Graphics.Gloss.Data.Color (white)
import Graphics.Gloss.Data.Display (Display (..))
import qualified Graphics.Gloss.Interface.Pure.Animate as G (animate)
import Graphics.Gloss.Data.Picture

import Control.Monad (when, void)
import Control.Applicative (pure, liftA2)

newtype TurtleCommand a = TurtleCommand 
  { 
    getSequence :: SequenceCommand (AlmostVal ()) a 
  }

instance Functor TurtleCommand where
  fmap f (TurtleCommand a) = TurtleCommand $ fmap f a

instance Applicative TurtleCommand where
  pure a = TurtleCommand $ pure a
  liftA2 f (TurtleCommand a) (TurtleCommand b) = TurtleCommand $ liftA2 f a b

instance Monad TurtleCommand where
  (TurtleCommand a) >>= f = TurtleCommand $ a >>= \s -> getSequence (f s)


instance Semigroup a => Semigroup (TurtleCommand a) where
  (TurtleCommand a) <> (TurtleCommand b) = 
    TurtleCommand $ combineSequence a b

{-
combine :: TurtleCommand () () -> TurtleCommand () () -> TurtleCommand () ()
combine (TurtleCommand a) (TurtleCommand b) = TurtleCommand $ combineSequence a b
-}

backward :: Turtle -> Float -> TurtleCommand ()
backward turtle d = forward turtle (-d)

forward :: Turtle -> Float -> TurtleCommand ()
forward turtle d = TurtleCommand $ forward_ turtle d

right :: Turtle -> Float -> TurtleCommand ()
right t r = left t (-r)

left :: Turtle -> Float -> TurtleCommand ()
left t r = TurtleCommand $ left_ t r

forward_ :: Turtle -> Float -> SequenceCommand (AlmostVal a) ()
forward_ turtle d = do
    t <- turtleData turtle
    -- ^ Get origin point
    let speed = _speed t
    animate' d speed $ \ q -> do
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

left_ :: Turtle -> Float -> SequenceCommand (AlmostVal a) ()
left_ turtle r = do
    t <- turtleData turtle
    let r' = normalizeDirection r
    animate' (r' * pi) (_speed t * 180) $ \q -> do
      let heading = _heading t
      --let q' = if r > 0 then q else -q
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

  -- | Return a valid heading value between (-180, 180).
normalizeDirection :: Float -> Float
normalizeDirection f
  | f < -r     = normalizeHeading (f + r)
  | f >  r     = normalizeHeading (f - r)
  | otherwise = f
  where r = 180

makeTurtle :: TurtleCommand Turtle
makeTurtle = TurtleCommand generateTurtle

runTurtle :: TurtleCommand () -> IO ()
runTurtle tc = G.animate display white iterate
     where display = InWindow "World Turtle" (800, 600) (400, 300)
           iterate f = renderTurtle (getSequence tc) f