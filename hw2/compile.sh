#!/usr/bin/env bash

rm -rf classes
mkdir classes
scalac -classpath classes/ -d classes/ src/Data.scala -feature
scalac -classpath classes/ -d classes/ src/Main.scala -feature
scalac -classpath classes/ -d classes/ src/Test.scala -feature
