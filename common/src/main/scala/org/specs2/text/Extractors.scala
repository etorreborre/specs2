package org.specs2
package text

import control.Exceptions._

trait Extractors {
  def anInt = (s: String) => "(\\d+)".r.findFirstIn(s).map(_.toInt)

  def twoInts = (s: String) => tryo {
    val ints = "(\\d+)".r.findAllIn(s).map(_.toInt).toIndexedSeq
    (ints(0), ints(1))
  }

  def stripAnInt = (s: String) => tryo {
    """\{([^}]+)\}""".r.findFirstIn(s).map { v =>
      (s.replace("{", "").replace("}", ""), v.replace("{", "").replace("}", "").toInt)
    }
  }.flatten

}
