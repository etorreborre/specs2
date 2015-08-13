package org.specs2.mutable

class MutableSpec extends Specification {

  "s2 strings must create examples".p
  s2"""${ this.is.examples must haveSize(1) }"""

}
