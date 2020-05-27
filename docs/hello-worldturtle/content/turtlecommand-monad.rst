#######################
The TurtleCommand Monad
#######################

There's many, /many/ tutorials out there on Monads. More than I can count. With
this in mind, I am not going to tell you  `Monads are not burritos`_, 
`Monads are burritos`_, or that 
`Monads are just monoids in the category of endofunctors`_.

Instead, let's talk about the TurtleCommand.

When we say 

.. code-block:: haskell
  t <- makeTurtle
  forward 90 t

We do two things:

* Create a new turtle.
* Move the turtle forward 90 units.

The type signatures are:

.. code-block:: haskell
  makeTurtle :: TurtleCommand Turtle
  forward :: Float -> Turtle -> TurtleCommand ()

:code:`makeTurtle` makes sense. Calling :code:`makeTurtle` returns a 
:code:`TurtleCommand Turtle`.

We run our turtle simulations by passing a TurtleCommand into `runTurtle`_. 

The type signature of `runTurtle`_ is this:

.. code-block:: haskell

  runTurtle :: TurtleCommand () -> IO ()

`runTurtle`_ clearly takes a TurtleCommand and does something with it. You're
not allowed to know. It's secret. It's forbidden knowledge 
(`it's right here<https://hackage.haskell.org/package/worldturtle-0.1.0.0/docs/src/Graphics.WorldTurtle.html#runTurtle>`_)
.


.. _Monads are burritos: https://blog.plover.com/prog/burritos.html

.. _Monads are not burritos: https://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/

.. _Monads are just monoids in the category of endofunctors: https://blog.merovius.de/2018/01/08/monads-are-just-monoids.html

.. _runTurtle: https://hackage.haskell.org/package/worldturtle-0.1.0.0/docs/Graphics-WorldTurtle.html#v:runTurtle