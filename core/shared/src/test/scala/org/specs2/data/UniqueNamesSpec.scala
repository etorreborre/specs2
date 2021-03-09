package org.specs2
package data

import mutable.Spec

class UniqueNamesSpec extends Spec:

  "Returned names must be unique" >> {
    val namer = UniqueNames()

    namer.uniqueName("hello") must ===("hello")
    namer.uniqueName("hello") must ===("hello_1")
    namer.uniqueName("world") must ===("world")
    namer.uniqueName("hello") must ===("hello_2")

  }
