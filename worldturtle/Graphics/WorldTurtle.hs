{- | 
    Module      : Graphics.WorldTurtle
    Description : WorldTurtle
    Copyright   : (c) Archibald Neil MacDonald, 2020
    License     : BSD3
    Maintainer  : FortOyer@hotmail.co.uk
    Stability   : experimental
    Portability : POSIX
  
    "Graphics.WorldTurtle" is a module for writing and rendering turtle graphics
    in Haskell.
  
    Take a look at the
         [examples](https://github.com/FortOyer/worldturtle-haskell#examples) on
    Github!
-}
module Graphics.WorldTurtle
     (
     -- * Running a WorldTurtle simulation.
     -- * Running on a single turtle.
     -- $running
       runTurtle
     , TurtleCommand
     -- * Running a world of turtles.
     -- $multiturtle
     , runWorld
     , WorldCommand
     , run 
     , (>/>)
     -- * Parallel animation
     -- $parallel
     , (<|>)
     -- * Stop an animation
     -- $empty
     , empty
     -- * Further documentation
     , module Graphics.WorldTurtle.Commands
     , module Graphics.WorldTurtle.Shapes
     , module Graphics.WorldTurtle.Color
     ) where

import Control.Applicative (empty, (<|>))

import Graphics.Gloss.Data.Display (Display (..))
import qualified Graphics.Gloss.Data.ViewState as G
import qualified Graphics.Gloss.Data.ViewPort as G
import qualified Graphics.Gloss.Interface.Pure.Game as G

import Graphics.WorldTurtle.Color
import Graphics.WorldTurtle.Commands
import Graphics.WorldTurtle.Internal.Sequence (renderTurtle)
import Graphics.WorldTurtle.Internal.Commands (TurtleCommand, seqT
                                              , WorldCommand (..), seqW)
import Graphics.WorldTurtle.Shapes

-- | Takes a `TurtleCommand` and executes the command on an implicitly created
--   turtle that starts at position @(0, 0)@ with heading `north`. 
--
--   This is a convenience function written in terms of `runWorld` as:
--
--   > runTurtle c = runWorld $ makeTurtle >>= run c
--
-- See also: `Graphics.WorldTurtle.Commands.makeTurtle`.
runTurtle :: TurtleCommand () -- ^ Command sequence to execute.
          -> IO ()
runTurtle c = runWorld $ makeTurtle >>= run c

{- | `runWorld` takes a `WorldCommand` and produces the animation in a new
      window! 

   ==  Interacting with the window.
   
   While running, you can interact with the window in the following way:
   
   +------------------------------------------+-------------------+
   | Action                                   | Interaction       |
   +==========================================+===================+
   | Pan the viewport.                        | Click and drag    |
   +------------------------------------------+-------------------+
   | Zoom in/out.                             |Mousewheel up/down |
   +------------------------------------------+-------------------+
   | Reset the viewport to initial position.  | Spacebar          |
   +------------------------------------------+-------------------+
   | Reset the animation.                     | @R@ key           |
   +------------------------------------------+-------------------+
   | Pause the animation.                     | @P@ key           |
   +------------------------------------------+-------------------+
   | Quit                                     | Escape key        |
   +------------------------------------------+-------------------+
-}
runWorld :: WorldCommand () -- ^ Command sequence to execute
          -> IO ()
runWorld tc = G.play display white 30 defaultWorld iterateRender input timePass
  where display = InWindow "World Turtle" (800, 600) (400, 300)
        iterateRender w = G.applyViewPortToPicture 
                               (G.viewStateViewPort $ state w)
                        $! renderTurtle (seqW tc) (elapsedTime w)
        input e w 
             -- Reset key resets sim state (including unpausing). We 
             -- deliberately keep view state the same.
             | isResetKey_ e = w { elapsedTime = 0, running = True }
             -- Pause prevents any proceeding.
             | isPauseKey_ e = w { running = not $ running w }
             -- Let Gloss consume the command.
             | otherwise = w { state = G.updateViewStateWithEvent e $ state w } 
        -- Increment simulation time if we are not paused.
        timePass f w
         | running w = w { elapsedTime = f + elapsedTime w }
         | otherwise = w

-- | `run` takes a `TurtleCommand` and a `Turtle` to execute the command on. 
--  The result of the computation is returned wrapped in a `WorldCommand`.
--
--  For example, to create  a turtle and get its @x@ `position` one might 
--  write:
--
--  >  myCommand :: Turtle -> WorldCommand Float
--  >  myCommand t = do
--  >    (x, _) <- run position t
--  >    return x
--
--  Or to create a command that accepts a turtle and draws a right angle:
--
--  > myCommand :: Turtle -> WorldCommand ()
--  > myCommand = run $ forward 10 >> right 90 >> forward 10
run :: TurtleCommand a -- ^ Command to execute
    -> Turtle -- ^ Turtle to apply the command upon.
    -> WorldCommand a -- ^ Result as a `WorldCommand`
run c = WorldCommand . seqT c

-- | This is an infix version of `run` where the arguments are swapped.
--
--   We take a turtle and a command to execute on the turtle.
--   The result of the computation is returned wrapped in a `WorldCommand`.
--
--   To create a turtle and draw a right-angle:
--
--   > myCommand :: WorldCommand ()
--   > myCommand = do
--   >   t <- makeTurtle
--   >   t >/> do 
--   >     forward 10
--   >     right 90
--   >     forward 10
(>/>) :: Turtle -- ^ Turtle to apply the command upon.
      -> TurtleCommand a -- ^ Command to execute
      -> WorldCommand a -- ^ Result as a `WorldCommand`
(>/>) = flip run
infixl 4 >/>

data World = World { elapsedTime :: !Float
                   , running :: !Bool
                   , state :: !G.ViewState 
                   }

defaultWorld :: World
defaultWorld = World 0 True 
             $ G.viewStateInitWithConfig 
             -- Easier to do this to have spacebar overwrite R.
             $ reverse 
             $ (G.CRestore, [(G.SpecialKey G.KeySpace, Nothing)])
             : G.defaultCommandConfig

-- | Tests to see if a key-event is the reset key.
isResetKey_ :: G.Event -> Bool
isResetKey_ (G.EventKey (G.Char 'r') G.Down _ _)  = True
isResetKey_ (G.EventKey (G.Char 'R') G.Down _ _)  = True
isResetKey_ _ = False

-- Tests to see if a key event is the pause key
isPauseKey_ :: G.Event -> Bool
isPauseKey_ (G.EventKey (G.Char 'p') G.Down _ _)  = True
isPauseKey_ (G.EventKey (G.Char 'P') G.Down _ _)  = True
isPauseKey_ _ = False

{- $running

  To start animating a single turtle, you just pass your commands to
  `runTurtle` like so:

  >    import Control.Monad (replicateM_)
  >    import Graphics.WorldTurtle
  >
  >    drawSquare :: Float -> TurtleCommand ()
  >    drawSquare size = replicateM_ 4 $ forward size >> right 90
  >
  >    main :: IO ()
  >    main = runTurtle $ drawSquare 100

   Which will produce this animation.

   ![basic_turtle_square gif](docs/images/basic_turtle_square.gif)
-}

{- $multiturtle
   
   For executing commands on multiple turtles, we use `runWorld` which
   executes on `WorldCommand`s. Here is an example where 2 turtles draw a
   circle independently:

   > import Graphics.WorldTurtle
   >
   > main :: IO ()
   > main = runWorld $ do
   >   t1 <- makeTurtle
   >   t2 <- makeTurtle
   > 
   >   t1 >/> circle 90 
   >   t2 >/> circle (-90)

   Notice that in a `WorldCommand` context we must create our own turtles with 
   `makeTurtle`! We them  apply the `TurtleCommand`
   on our turtles using the run operator `(>/>)`.
-}

{- $parallel

   #parallel#

   While `WorldCommand`s can be combined with `(>>)` to produce sequential
   instructions, we can also use the
   alternative operator `(<|>)` to achieve parallel instructions. That is: 
   animate two turtles at time!

   Here is an example:

   >  import Graphics.WorldTurtle
   >
   >  main :: IO ()
   >  main = runWorld $ do
   >    t1 <- makeTurtle' (0, 0) north green
   >    t2 <- makeTurtle' (0, 0) north red
   >
   >    -- Draw the anticlockwise and clockwise circles in sequence. 
   >    t1 >/> circle 90 >> t2 >/> circle (-90)
   >  
   >    clear
   >
   >    -- Draw the anticlockwise and clockwise circles in parallel.
   >    t1 >/> circle 90 <|> t2 >/> circle (-90)

   Which would produce an animation like this

   ![parallel and serial gif](docs/images/parallel_serial_turtles_2.gif)

   Note that the result of @x \<|\> y@ is:
   
   >>> x <|> y
   x

   when @x@ is not `Control.Applicative.empty`, otherwise the result is @y@.
-}

{- $empty
   If a `WorldCommand` is `Control.Applicative.empty`, then this stops this 
   section of  animation and it does not progress. To this end 
   `Control.Monad.guard` can be used to calculate when to stop part of an 
   animation sequence.
-}