package org.specs2
package control
import Identityx._

class IdentityxSpec extends mutable.Specification {

  "A function can be executed conditionally on an object depending on an implicit condition" >> {
    "when the condition is true" >> {
      implicit val doIt = true
      "hello" ?> (_.drop(1)) must_== "ello"
    }
    "when the condition is false" >> {
      implicit val doIt = false
      "hello" ?> (_.drop(1)) must_== "hello"
    }
  }
  "A function can be executed conditionally on an object depending on an explicit condition" >> {
    "when the condition is true" >> {
      "hello".when(true) (_.drop(1)) must_== "ello"
    }
    "when the condition is false" >> {
      "hello".when(false) (_.drop(1)) must_== "hello"
    }
  }
}