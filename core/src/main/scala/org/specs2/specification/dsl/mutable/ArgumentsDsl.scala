package org.specs2
package specification
package dsl
package mutable

import main._


/**
 * This trait provides shortcuts to create Arguments instances and adding them to the SpecificationStructure by mutating its
 * current content
 */
trait ArgumentsDsl extends ArgumentsArgs with MutableArgumentsBuilder {
  /** shorthand method to create an Arguments object */
  override def args(ex:            ArgProperty[String]            = ArgProperty[String](),
                    include:       ArgProperty[String]            = ArgProperty[String](),
                    exclude:       ArgProperty[String]            = ArgProperty[String](),
                    @deprecated("use the was x! instead", since="3.0")
                    wasIssue:      ArgProperty[Boolean]           = ArgProperty[Boolean](),
                    was:           ArgProperty[String]            = ArgProperty[String](),
                    plan:          ArgProperty[Boolean]           = ArgProperty[Boolean](),
                    skipAll:       ArgProperty[Boolean]           = ArgProperty[Boolean](),
                    stopOnFail:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
                    stopOnSkip:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
                    sequential:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
                    isolated:      ArgProperty[Boolean]           = ArgProperty[Boolean](),
                    @deprecated("use the org.specs2.specification.process.RandomSequentialExecution trait instead", since="3.0")
                    random:        ArgProperty[Boolean]           = ArgProperty[Boolean](),
                    xonly:         ArgProperty[Boolean]           = ArgProperty[Boolean](),
                    showOnly:      ArgProperty[String]            = ArgProperty[String](),
                    color:         ArgProperty[Boolean]           = ArgProperty[Boolean]()) =

    setArguments(super.args(
      ex,
      include,
      exclude,
      wasIssue,
      was,
      plan,
      skipAll,
      stopOnFail,
      stopOnSkip,
      sequential,
      isolated,
      random,
      xonly,
      showOnly,
      color))

}



