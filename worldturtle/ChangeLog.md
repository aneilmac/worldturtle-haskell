# Changelog for turtle-haskell

## v0.3.1.0

* Fixed longstanding bug where turtle rotation did not 
  rotate at the advertised rate.
* Fixed longstanding bug where rotation did not accept negative
  values gracefully.
* Tweaked intial turtle rotation speed to 720 degrees/sec.
* `goto` command now animates a transition.
* Removed `setPosition` function.

## v0.3.0.0

* Upgraded to `lts-18.27`.
* Added `runWorld'` and `runTurtle'` variant commands which take a background color.
* Deprecated `setPosition`.
* Added `jump`, which is a variant of `goto` which never draws a line.
* Added `wait` command, which is a `TurtleCommand` variant of `sleep`.
* Added `label` and `label'` commands, which allows text to be drawn at turtle's position.
* Added `repeatFor` method which is an alias for `Control.Monad.replicateM_` (this is purely 
  to help ease students into Monad concepts.)
* `TurtleCommand` and `WorldCommand` are now instances of `MonadIO`.
* Major internal performance improvements. `SequenceCommand`, is now a `Coroutine`. 
  This reduces wasted calculations per-frame as the state of the previous frame 
  can now be carried into the next frame of animation.
* Removed `WorldCommand` as an instance of `Control.Applicative` and `MonadPlus`. This did not
  make sense in terms of parallelization. Instead, `WorldComamnd` is now an instance of `MonadParallel`
* Introduced new `>!>` operator for parallel animations.
* `setPenDown` has been split into `setPenDown` and `setPenUp` to be more LOGO-like.
* `setVisible` has been split into `setVisible` and `setInvisible` to be more LOGO-like.
* Added the `labelwait-exe` test.
* Updated examples to account for command changes.
* Removed `spaceleak-exe` test.

## v0.2.2.1

* Upgrading upper bounds of the lens package to allow for compilation with GHC
  9.0.1.
* Regenerated `.travis.yml` to reflect new GHC supported version.

## v0.2.2

* Upgrading to GHC 8.10.3 to resolve problems GHC compiler problems with Mac 
OSX. See [here](https://gitlab.haskell.org/ghc/ghc/-/issues/18446) for details.
* Fixed issue where newly drawn lines were drawn under older lines.

## v0.2.1

* Internally simplified the commands system to use a Maybe Monad for sequencing.

## v0.2.0

* Split `TurtleCommand` into `TurtleCommand` and `WorldCommand` to help reduce
  boilerplate of having to apply a turtle to a command for every stage of a
  command block.
* Added `runWorld`, `runTurtle`, `run`, and `(>/>)` functions.
* `circle` command split into `arc` and `circle` commands.
* Fixed `shiftHue` as function did not match documentation.

## v0.1.2

* Added the `branch` function.

## v0.1.1

* Added `sleep` function.
* Added `rotationSpeed` function.
* Added the `shiftHue` color function.
* Fixed bugs in circle rendering when going in a clockwise direction.
* Fixed the bounds and potential recursion pitfall in internal normalization
  functions.
* If rotation left 270 degrees will not turn right 90 degrees and vice versa.

## v0.1.0

Initial release.
