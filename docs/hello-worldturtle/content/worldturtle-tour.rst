####################
The WorldTurtle tour
####################

*****************
What are Turtles?
*****************

Turtles are programmable cursors that when given a few simple commands can draw
cool, complex shapes! 

.. figure:: /_static/images/physical_turtle_draw.jpg
  :width: 300px
  :figclass: align-center
  :alt: Robot turtle drawing on paper.

  We call our cursors "turtles" after the original, programmable robots!

  Attribution: Valiant Technology Ltd.

Turtle graphics lends themselves well to Haskell concepts like recursion and
monads. This series is a collection of tutorials for the beginner to learn
Haskell using the WorldTurtle module.

.. figure:: /_static/images/branch_output.gif
  :width: 300px
  :figclass: align-center
  :alt: Turtle graphics drawing a tree.

  A tree drawn recursively using turtle graphics.

**********
Off we go!
**********

Let's :ref:`build a new project<Creating a new WorldTurtle project>` called
:code:`my-first-turtle`!

.. note::
  
  To create a new project called "my-first-turtle" with Stack type the 
  following in the command line!

  .. code-block:: bash

    stack new my-first-turtle FortOyer/worldturtle

Inside :code:`my-first-turtle/src/Main.hs` is the code for your new WorldTurtle
application.

.. code-block:: haskell

  module Main where

  import Graphics.WorldTurtle

  main :: IO ()
  main = runTurtle $ do
    t <- makeTurtle
    forward 90 t

