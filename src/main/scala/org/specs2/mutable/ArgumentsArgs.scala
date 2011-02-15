package org.specs2
package mutable

import control.Property
import main._
import specification.Fragments

/**
 * This trait provides shortcuts to create Arguments instances and adding them to the BaseSpecification by mutating its
 * current content
 */
trait ArgumentsArgs extends main.ArgumentsArgs { this: BaseSpecification =>
  /** shorthand method to create an Arguments object */
  override def args(
    ex:            Property[String]   = Property[String](),
    xonly:         Property[Boolean]  = Property[Boolean](),
    plan:          Property[Boolean]  = Property[Boolean](),
    failtrace:     Property[Boolean]  = Property[Boolean](),
    color:         Property[Boolean]  = Property[Boolean](),
    noindent:      Property[Boolean]  = Property[Boolean](),
    showlevel:     Property[Boolean]  = Property[Boolean](),
    showtimes:     Property[Boolean]  = Property[Boolean](),
    offset:        Property[Int]      = Property[Int](),
    specName:      Property[String]   = Property[String](),
    sequential:    Property[Boolean]  = Property[Boolean](),
    threadsNb:     Property[Int]      = Property[Int](),
    markdown:      Property[Boolean]  = Property[Boolean](),
    debugMarkdown: Property[Boolean]  = Property[Boolean](),
    diffs:         Property[Diffs]    = Property[Diffs](),
    fromSource:    Property[Boolean]  = Property[Boolean]()
  ) = {
    addArguments(super.args(
      ex,
      xonly,
      plan,
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
      fromSource))
  }

}
