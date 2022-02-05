#!/bin/sh
test -e ~/.coursier/cr || (mkdir -p ~/.coursier && wget -q -O ~/.coursier/cr https://git.io/vgvpD && chmod +x ~/.coursier/cr)
CLASSPATH="$(~/.coursier/cr fetch -q -p \
  \
  org.specs2:specs2-core_3:5.0.0 \
  com.lihaoyi:ammonite-repl_2.13.8:2.5.2 \
  \
)" java ammonite.repl.Main --predef 'import org.specs2._'
