package org.specs2
package data

import mutable.Specification

class UniqueNamesSpec extends Specification {
  "Returned names must be unique" >> {
    val namer = UniqueNames()

    namer.uniqueName("hello") === "hello"
    namer.uniqueName("hello") === "hello_1"
    namer.uniqueName("world") === "world"
    namer.uniqueName("hello") === "hello_2"

  }
}
