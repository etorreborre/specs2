package org.specs2
package main

import control.Property

/**
 * This trait provides shortcuts to create Arguments instances
 */
trait ArgumentsArgs extends control.Properties {
  /** shorthand method to create an Arguments object */
  def args(
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
    debugMarkdown: Property[Boolean]  = Property[Boolean]()
  ) = new Arguments(
     ex.map(".*"+_+".*").toOption,
     xonly.toOption,
     plan.toOption,
     failtrace.toOption,
     color.toOption,
     noindent.toOption,
     showlevel.toOption,
     showtimes.toOption,
     offset.toOption,
     specName.toOption,
     sequential.toOption,
     threadsNb.toOption,
     markdown.toOption,
     debugMarkdown.toOption
  )
  /**
   * @return arguments for a literate specification: no auto indent and a sequential
   *         execution
   */
  def literate: Arguments = args(noindent = true, sequential = true)
  /**
   * @return arguments for a specification where examples must be executed sequentially
   */
  def sequential: Arguments = args(sequential = true)
  /**
   * shortcut to show only the text without any execution
   */
  def plan: Arguments = args(plan = true)
  /**
   * shortcut to avoid automatic indentation
   */
  def noindent = args(noindent = true)
  /**
   * shortcut to not executing the text and avoid automatic indentation
   */
  def freetext: Arguments = plan <| noindent
  /**
   * shortcut to print only failures and errors
   */
  def xonly: Arguments = args(xonly = true)
  /**
   * shortcut to executed and print only some examples
   */
  def only(examples: String): Arguments = args(ex = examples)
}
object ArgumentsArgs extends ArgumentsArgs
