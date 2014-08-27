package org.specs2
package specification
package dsl
package mutable

import main._

/**
 * Arguments creation with an additional implicit from (=> T) to Property[T] to allow the direct passing of parameters
 */
trait ArgumentsDsl extends ArgumentsCreation with ArgProperties

/**
 * Methods with default Property values to create Arguments instances
 * Arguments are being added to the SpecificationStructure by mutating its
 * current content
 */
trait ArgumentsCreation extends org.specs2.main.ArgumentsCreation with MutableArgumentsBuilder {
  /** shorthand method to create an Arguments object */
  override def args(ex:            ArgProperty[String]            = ArgProperty[String](),
                    include:       ArgProperty[String]            = ArgProperty[String](),
                    exclude:       ArgProperty[String]            = ArgProperty[String](),
                    was:           ArgProperty[String]            = ArgProperty[String](),
                    plan:          ArgProperty[Boolean]           = ArgProperty[Boolean](),
                    skipAll:       ArgProperty[Boolean]           = ArgProperty[Boolean](),
                    stopOnFail:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
                    stopOnSkip:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
                    sequential:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
                    isolated:      ArgProperty[Boolean]           = ArgProperty[Boolean](),
                    xonly:         ArgProperty[Boolean]           = ArgProperty[Boolean](),
                    showOnly:      ArgProperty[String]            = ArgProperty[String](),
                    color:         ArgProperty[Boolean]           = ArgProperty[Boolean]()) =

    setArguments(super.args(
      ex,
      include,
      exclude,
      was,
      plan,
      skipAll,
      stopOnFail,
      stopOnSkip,
      sequential,
      isolated,
      xonly,
      showOnly,
      color))

}



