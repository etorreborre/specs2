package org.specs2
package text

import mutable.Specification
import Message._

class MessageSpec extends Specification {

  "it is possible to concat messages, so that empty messages don't get displayed" >> {
    concat("hello", "", ",")      === "hello"
    concat("hello", "world", ",") === "hello,world"
    concat("", "world", ",")      === "world"
  }

}