package org.specs2
package text

import mutable.Spec
import Message.*

class MessageSpec extends Spec {

  "it is possible to concat messages, so that empty messages don't get displayed" >> {
    concat("hello", "", ",")      must ===("hello")
    concat("hello", "world", ",") must ===("hello,world")
    concat("", "world", ",")      must ===("world")
  }

}
