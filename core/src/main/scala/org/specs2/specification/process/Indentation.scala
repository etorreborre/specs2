package org.specs2
package specification
package process

import scala.math._
import specification.core._

trait Indentation {

  def fold = (fragment: Fragment, indentation: Int) =>
    indentation +
    (fragment match {
      case f @ Fragment(tab(n),_ ,_)     => indentation + 1
      case f @ Fragment(backtab(n),_ ,_) => max(0, indentation - 1)
      case _                             => indentation
    })

}

object Indentation extends Indentation
