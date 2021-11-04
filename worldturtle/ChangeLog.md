# Changelog for turtle-haskell

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
