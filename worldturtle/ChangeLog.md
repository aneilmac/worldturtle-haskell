# Changelog for turtle-haskell

## v0.2.0

* Split `TurtleCommand` into `WorldCommand` and `WorldCommand` to help reduce
  boilerplate of having to apply a turtle to a command for every stage of a
  command block.
* Added `runWorld`, `runTurtle`, `run`, and `(>/>)` functions.
* `circle` command split into `arc` and `circle` commands.

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

## Unreleased changes
