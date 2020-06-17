#!/bin/bash
rm --force $1{.aux,.hp,.pdf,.ps} && \
  stack exec --profile -- $1 -RTS ${@:2} +RTS -hd -L100 \
  && stack exec -- hp2ps -c -b -d $1.hp && ps2pdf $1.ps
