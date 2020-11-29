package org.specs2
package mutable

import specification.dsl.mutable.{ExtendedExampleDsl, TagDsl}
import control.Use

class SpecSpec extends Spec with TagDsl:
  sequential

  "A mutable Spec can use additional traits to expand its dsl" >> {
    val s = new Spec with ExtendedExampleDsl {
      "hello" >> ok
    }
    Use(s)
    ok
  }
