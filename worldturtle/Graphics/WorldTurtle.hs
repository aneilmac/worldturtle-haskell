{- | 
    Module      : Graphics.WorldTurtle
    Description : WorldTurtle
    Copyright   : (c) Archibald Neil MacDonald, 2020
    License     : BSD3
    Maintainer  : archibaldnmac@gmail.com
    Stability   : experimental
    Portability : POSIX
  
    "Graphics.WorldTurtle" is a module for writing and rendering turtle graphics
    in Haskell.
  
    Take a look at the
         [examples](https://github.com/aneilmac/worldturtle-haskell#examples) on
    Github!
-}
module Graphics.WorldTurtle
     (
     -- * Running a WorldTurtle simulation.
     -- * Running on a single turtle.
     -- $running
       runTurtle
     , runTurtle'
     , TurtleCommand
     -- * Running a world of turtles.
     -- $multiturtle
     , runWorld
     , runWorld'
     , WorldCommand
     , run 
     , (>/>)
     -- * Parallel animation
     , (>!>)
     -- * Further documentation
     , module Graphics.WorldTurtle.Commands
     , module Graphics.WorldTurtle.Shapes
     , module Graphics.WorldTurtle.Color
     ) where

import Control.Monad.Parallel

import Graphics.Gloss.Data.Display (Display (..))
import qualified Graphics.Gloss.Data.ViewState as G
import qualified Graphics.Gloss.Data.ViewPort as G
import qualified Graphics.Gloss.Interface.IO.Game as G

import Graphics.WorldTurtle.Color
import Graphics.WorldTurtle.Commands
import Graphics.WorldTurtle.Internal.Sequence (SequencePause, startSequence, resumeSequence, renderPause, defaultTSC)
import Graphics.WorldTurtle.Internal.Commands ( TurtleCommand
                                              , WorldCommand (..)
                                              , run
                                              , seqW
                                              )
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
runTurtle = runTurtle' white

-- | Variant of `runTurtle` which takes an additional background color parameter. 
runTurtle' :: Color -- ^ Background color.
          -> TurtleCommand () -- ^ Command sequence to execute.
          -> IO ()
runTurtle' bckCol c = runWorld' bckCol $ makeTurtle >>= run c

-- | While `WorldCommand`s can be combined with `(>>)` to produce sequential
--   instructions, we can also use the
--   parallel animation operator `(>!>)` to achieve parallel instructions. 
--   That is: animate two turtles at time!
--
--   Here is an example:
--
--   >  import Graphics.WorldTurtle
--   >
--   >  main :: IO ()
--   >  main = runWorld $ do
--   >    t1 <- makeTurtle' (0, 0) north green
--   >    t2 <- makeTurtle' (0, 0) north red
--   >
--   >    -- Draw the anticlockwise and clockwise circles in sequence. 
--   >    t1 >/> circle 90 >> t2 >/> circle (-90)
--   >  
--   >    clear
--   >
--   >    -- Draw the anticlockwise and clockwise circles in parallel.
--   >    t1 >/> circle 90 >!> t2 >/> circle (-90)
--
--   Which would produce an animation like this
--
-- ![parallel and serial gif](docs/images/parallel_serial_turtles_2.gif)
--
-- Note that `(>!>)` is an alias for `bindM2`, and is defined as:
-- 
-- >  (>!>) = bindM2 (const . return)
--
(>!>) :: WorldCommand () -- ^ First command to execute in parallel
      -> WorldCommand () -- ^ Second command to execute in parallel.
      -> WorldCommand () -- ^ Result command
(>!>) = bindM2 (const . return)
infixl 3 >!>

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
runWorld = runWorld' white

-- | Variant of `runWorld` which takes an additional background color parameter. 
runWorld' :: Color -- ^ Background color
          -> WorldCommand () -- ^ Command sequence to execute
          -> IO ()
runWorld' bckCol cmd = G.playIO display bckCol 30 (defaultWorld cmd) iterateRender input timePass
  where display = InWindow "World Turtle" (800, 600) (400, 300)
        iterateRender w = do
           sq <- worldComputation w
           let p = renderPause sq -- Render whatever is in the coroutine.
           return $ G.applyViewPortToPicture (G.viewStateViewPort $ viewState w) p
        input e w 
             -- Reset key resets sim state (including unpausing). We 
             -- deliberately keep view state the same.
             | isResetKey_ e = return w {worldComputation = restartSequence cmd, running = True }
             -- Pause prevents any proceeding.
             | isPauseKey_ e = return w { running = not $ running w }
             -- Let Gloss consume the command.
             | otherwise = return w { viewState = G.updateViewStateWithEvent e $ viewState w } 
        -- Increment simulation time if we are not paused.
        timePass f w
         | running w = do
               sq <- worldComputation w -- Grab previous sequence
               sq' <- resumeSequence f sq -- Calculate new sequence
               return w { worldComputation = return sq'}
         | otherwise = return w

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

data World a = World { running :: !Bool
                     , worldComputation:: IO (SequencePause a)
                     , viewState :: G.ViewState
                     }

restartSequence :: WorldCommand a -> IO (SequencePause a)
restartSequence cmnd = startSequence defaultTSC (seqW cmnd)

defaultWorld :: WorldCommand a -> World a
defaultWorld cmd = World True (restartSequence cmd)
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

  >    import Graphics.WorldTurtle
  >
  >    drawSquare :: Float -> TurtleCommand ()
  >    drawSquare size = repeatFor 4 $ forward size >> right 90
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
