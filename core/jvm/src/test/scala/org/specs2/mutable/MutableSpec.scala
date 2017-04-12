package org.specs2
package mutable

import org.specs2.concurrent.ExecutionEnv

class MutableSpec(implicit ee: ExecutionEnv) extends Specification {

  "s2 strings must create examples".p
  s2"""${ this.is.examplesList must haveSize(1) }"""

}
