package org.specs2
package guide

object OtherBuildTools extends UserGuidePage { def is = s2"""
The most straightforward way to run specs2 specifications is to use [sbt](http://scala-sbt.org). However other build tools such as Maven and Gradle can be used too (please refer to the ${"Installation" ~ Installation} guide for instructions on how to set-up projects for those tools).

## Maven

With maven you can run specifications with the `test` command.

## Gradle
"""
}

