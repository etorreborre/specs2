package org.specs2
package data

import mutable.Spec

class UniqueNamesSpec extends Spec {

  "Returned names must be unique" >> {
    val namer = UniqueNames()

    namer.uniqueName("hello") must_== "hello"
    namer.uniqueName("hello") must_== "hello_1"
    namer.uniqueName("world") must_== "world"
    namer.uniqueName("hello") must_== "hello_2"

  }

}
