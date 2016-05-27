#!/bin/sh
test -e ~/.coursier/cr || (mkdir -p ~/.coursier && wget -q -O ~/.coursier/cr https://git.io/vgvpD && chmod +x ~/.coursier/cr)
CLASSPATH="$(~/.coursier/cr fetch -q -p \
  \
  org.specs2:specs2-core_2.11:3.8.3 \
  com.lihaoyi:ammonite-repl_2.11.7:0.5.2 \
  \
)" java ammonite.repl.Main --predef 'import org.specs2._'
