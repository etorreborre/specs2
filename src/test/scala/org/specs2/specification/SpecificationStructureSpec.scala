package org.specs2
package specification

import SpecificationStructure._
import matcher.DataTables

class SpecificationStructureSpec extends mutable.Specification with DataTables {

  "It is possible to create a specification from a class name" >> {
    "name"                            ||  "comment"                                       |>
    "AnImmutableSpecification"        !!  "an immutable spec class"                       |
    "AMutableSpecification"           !!  "a mutable spec class"                          |
    "AnImmutableSpecificationObject$" !!  "an immutable spec object"                      |
    "AMutableSpecificationObject$"    !!  "a mutable spec object"                         |
    "AnImmutableSpecificationObject"  !!  "an immutable spec object without a trailing $" |
    "AMutableSpecificationObject"     !!  "a mutable spec object without a trailing $"    | { (name, comment) =>
      createSpecificationOption("org.specs2.specification."+name) aka comment must beSome
    }
  }
}

class AnImmutableSpecification extends Specification { def is = ok }
class AMutableSpecification extends mutable.Specification { "this is" >> ok }
object AnImmutableSpecificationObject extends Specification { def is = ok }
object AMutableSpecificationObject extends mutable.Specification { "this is" >> ok }