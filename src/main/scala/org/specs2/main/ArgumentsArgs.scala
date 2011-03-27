package org.specs2
package main

import control.Property

/**
 * This trait provides shortcuts to create Arguments instances
 */
trait ArgumentsArgs extends ArgProperties {
  /** shorthand method to create an Arguments object */
  def args(
    ex:            ArgProperty[String]      = ArgProperty[String](),
    xonly:         ArgProperty[Boolean]     = ArgProperty[Boolean](),
    include:       ArgProperty[String]      = ArgProperty[String](),
    exclude:       ArgProperty[String]      = ArgProperty[String](),
    plan:          ArgProperty[Boolean]     = ArgProperty[Boolean](),
    skipAll:       ArgProperty[Boolean]     = ArgProperty[Boolean](),
    failtrace:     ArgProperty[Boolean]     = ArgProperty[Boolean](),
    color:         ArgProperty[Boolean]     = ArgProperty[Boolean](),
    noindent:      ArgProperty[Boolean]     = ArgProperty[Boolean](),
    showlevel:     ArgProperty[Boolean]     = ArgProperty[Boolean](),
    showtimes:     ArgProperty[Boolean]     = ArgProperty[Boolean](),
    offset:        ArgProperty[Int]         = ArgProperty[Int](),
    specName:      ArgProperty[String]      = ArgProperty[String](),
    sequential:    ArgProperty[Boolean]     = ArgProperty[Boolean](),
    threadsNb:     ArgProperty[Int]         = ArgProperty[Int](),
    markdown:      ArgProperty[Boolean]     = ArgProperty[Boolean](),
    debugMarkdown: ArgProperty[Boolean]     = ArgProperty[Boolean](),
    diffs:         ArgProperty[Diffs]       = ArgProperty[Diffs](),
    fromSource:    ArgProperty[Boolean]     = ArgProperty[Boolean](),
    commandLine:   Seq[String]              = Nil
  ) = new Arguments(
     ex.toOption.map(".*"+_+".*"),
     xonly.toOption,
     include.toOption,
     exclude.toOption,
     plan.toOption,
     skipAll.toOption,
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
     debugMarkdown.toOption,
     diffs.toOption,
     fromSource.toOption,
     commandLine
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
   * shortcut to skip all examples
   */
  def skipAll: Arguments = args(skipAll = true)
  /**
   * shortcut to avoid automatic indentation
   */
  def noindent: Arguments = args(noindent = true)
  /**
   * shortcut to not executing the text and avoid automatic indentation
   */
  def freetext: Arguments = plan <| noindent
  /**
   * shortcut to print only failures and errors
   */
  def xonly: Arguments = args(xonly = true)
  /**
   * shortcut to execute and print only some examples
   */
  def only(examples: String): Arguments = args(ex = examples)
  /**
   * shortcut to include only some tagged fragments
   */
  def include(tags: String): Arguments = args(include = tags)
  /**
   * shortcut to exclude some tagged fragments
   */
  def exclude(tags: String): Arguments = args(exclude = tags)
  /**
   * shortcut to display the differences with some specific parameters
   */
  def diffs(show: Boolean = true, separators: String = "[]", triggerSize: Int = 20, shortenSize: Int = 5, full: Boolean = false): Arguments =
    args(diffs = Diffs(show, separators, triggerSize, shortenSize, full))
  /**
   * shortcut to display the example descriptions from the expectations ok messages
   */
  def descFromExpectations = args(fromSource = false)
}
object ArgumentsArgs extends ArgumentsArgs

private[specs2]
trait ArgProperties {
  implicit def anyToArgProperty[T](t: =>T): ArgProperty[T] = ArgProperty(Property(t))
}
private[specs2]
object ArgProperties extends ArgProperties

private[specs2]
case class ArgProperty[T](p: Property[T] = Property[T]()) {
  def toOption: Option[T] = p.toOption
}