#!/bin/bash
rm --force $1{.aux,.hp,.pdf,.ps} && \
  stack exec --profile -- $1 -RTS ${@:2} +RTS -i0.02 -hc -hm\
Graphics.WorldTurtle,\
Graphics.WorldTurtle.Commands,\
Graphics.WorldTurtle.Internal.Commands,\
Graphics.WorldTurtle.Internal.Coords,\
Graphics.WorldTurtle.Internal.Sequence,\
Graphics.WorldTurtle.Internal.Turtle,\
Graphics.WorldTurtle.Shapes\
   && stack exec -- hp2ps -c $1.hp && ps2pdf $1.ps
  
