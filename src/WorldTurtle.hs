module WorldTurtle
     ( runTurtle
     , module WorldTurtle.Commands
     ) where

import Graphics.Gloss.Data.Color (white)
import Graphics.Gloss.Interface.Pure.Animate (animate)
import Graphics.Gloss.Data.Picture (Picture)
import Graphics.Gloss.Data.Display (Display(..))

import WorldTurtle.Commands
import WorldTurtle.Internal.Commands (renderTurtle)

runTurtle :: TurtleCommand () -> IO ()
runTurtle tc = animate display white iterate
     where display = InWindow "World Turtle" (800, 600) (400, 300)
           iterate f = renderTurtle tc f