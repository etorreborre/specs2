package org.specs2
package fp

import syntax.*

class IdentityxSpec extends mutable.Spec {

  "A function can be executed conditionally on an object depending on an explicit condition" >> {
    "when the condition is true" >> {
      "hello".orWhen(true)(_.drop(1)) must ===("ello")
    }
    "when the condition is false" >> {
      "hello".orWhen(false)(_.drop(1)) must ===("hello")
    }
  }
}
