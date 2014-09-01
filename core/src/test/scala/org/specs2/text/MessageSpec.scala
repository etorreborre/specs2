package org.specs2
package text

import mutable.Spec
import Message._

class MessageSpec extends Spec {

  "it is possible to concat messages, so that empty messages don't get displayed" >> {
    concat("hello", "", ",")      must_== "hello"
    concat("hello", "world", ",") must_== "hello,world"
    concat("", "world", ",")      must_== "world"
  }

}