package org.specs2
package mutable

import control.Property
import main._
import specification.Fragments

/**
 * This trait provides shortcuts to create Arguments instances and adding them to the SpecificationStructure by mutating its
 * current content
 */
trait ArgumentsArgs extends main.ArgumentsArgs { this: FragmentsBuilder =>
  /** shorthand method to create an Arguments object */
  override def args(
    ex:            ArgProperty[String]  = ArgProperty[String](),
    xonly:         ArgProperty[Boolean] = ArgProperty[Boolean](),
    plan:          ArgProperty[Boolean] = ArgProperty[Boolean](),
    skipAll:       ArgProperty[Boolean] = ArgProperty[Boolean](),
    failtrace:     ArgProperty[Boolean] = ArgProperty[Boolean](),
    color:         ArgProperty[Boolean] = ArgProperty[Boolean](),
    noindent:      ArgProperty[Boolean] = ArgProperty[Boolean](),
    showlevel:     ArgProperty[Boolean] = ArgProperty[Boolean](),
    showtimes:     ArgProperty[Boolean] = ArgProperty[Boolean](),
    offset:        ArgProperty[Int]     = ArgProperty[Int](),
    specName:      ArgProperty[String]  = ArgProperty[String](),
    sequential:    ArgProperty[Boolean] = ArgProperty[Boolean](),
    threadsNb:     ArgProperty[Int]     = ArgProperty[Int](),
    markdown:      ArgProperty[Boolean] = ArgProperty[Boolean](),
    debugMarkdown: ArgProperty[Boolean] = ArgProperty[Boolean](),
    diffs:         ArgProperty[Diffs]   = ArgProperty[Diffs](),
    fromSource:    ArgProperty[Boolean] = ArgProperty[Boolean](),
    commandLine:   Seq[String]          = Nil
  ) = {
    addArguments(super.args(
      ex,
      xonly,
      plan,
      skipAll,
      failtrace,
      color,
      noindent,
      showlevel,
      showtimes,
      offset,
      specName,
      sequential,
      threadsNb,
      markdown,
      debugMarkdown,
      diffs,
      fromSource,
      commandLine))
  }

}
