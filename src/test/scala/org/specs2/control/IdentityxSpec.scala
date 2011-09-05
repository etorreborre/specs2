package org.specs2
package control
import Identityx._

class IdentityxSpec extends mutable.Specification {
  "A function can be executed conditionally on an object depending on an implicit parameter" >> {
    "when the condition is true" >> {
      implicit val doIt = true
      "hello" ?> (_.drop(1)) must_== "ello"
    }

    "when the condition is false" >> {
      implicit val doIt = false
      "hello" ?> (_.drop(1)) must_== "hello"
    }

  }
}