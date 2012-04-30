package org.specs2
package specification

import SpecificationStructure._
import main.{CommandLineArguments, Arguments}
import matcher.{ResultMatchers, DataTables}

class SpecificationStructureSpec extends mutable.Specification with DataTables with ResultMatchers {

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

  "It is possible to create a specification from a class name with a constructor accepting an Arguments object" >> {
    forall(Seq("AnImmutableClassUsingArguments", "AMutableClassUsingArguments")) { name =>
      createSpecificationOption("org.specs2.specification."+name) must beSome
    }
  }

  "A specification can be defined with a trait providing command line arguments" >> {
    val spec = new CommandedSpecification
    spec.set(sequential)
    spec.content.examples(0).execute must beSuccessful
  }
  "A mutable specification can be defined with a trait providing command line arguments" >> {
    val spec = new MutableCommandedSpecification
    spec.set(sequential)
    spec.content.examples(0).execute must beSuccessful
  }

}

class AnImmutableSpecification extends Specification { def is = ok }
class AMutableSpecification extends mutable.Specification { "this is" >> ok }
object AnImmutableSpecificationObject extends Specification { def is = ok }
object AMutableSpecificationObject extends mutable.Specification { "this is" >> ok }

class AnImmutableClassUsingArguments(implicit args: Arguments) extends Specification { def is = ok }
class AMutableClassUsingArguments(implicit args: Arguments) extends mutable.Specification { "this is" >> ok }

class CommandedSpecification extends Specification with CommandLineArguments { def is =
  "this is" ! { arguments.sequential must beTrue }
}

class MutableCommandedSpecification extends mutable.Specification with CommandLineArguments {
  if (arguments.sequential) "this is" >> ok
  else                      "this is" >> ko
}