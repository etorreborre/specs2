package org.specs2
package specification
package core

import main.Arguments
import scalaz.concurrent.Task
import scalaz.stream._

/**
 * Structure of a Specification:
 *
 *  - a header
 *  - some arguments
 *  - specification fragments
 *
 * Note that the fragments have to be lazy in order to avoid cycles when 2 specifications are referencing
 * each other with links
 */
case class SpecStructure(header: SpecHeader, arguments: Arguments, lazyFragments: () => Fragments) {
  lazy val fragments = lazyFragments()

  def contents: Process[Task, Fragment]                                        = fragments.contents
  def map(f: Fragments => Fragments): SpecStructure                            = copy(lazyFragments = () => f(fragments))
  def |>(p: Process1[Fragment, Fragment]): SpecStructure                       = copy(lazyFragments = () => fragments |> p)
  def |>(f: Process[Task, Fragment] => Process[Task, Fragment]): SpecStructure = copy(lazyFragments = () => fragments update f)
  def flatMap(f: Fragment => Process[Task, Fragment]): SpecStructure           = |>(_.flatMap(f))

  def setHeader(h: SpecHeader) = copy(header = h)
  def setArguments(args: Arguments) = copy(arguments = args)
  def setFragments(fs: =>Fragments) = copy(lazyFragments = () => fs)

  def specClassName = header.className
  def name = header.title.getOrElse(header.simpleName)
  def wordsTitle = header.title.getOrElse(header.wordsTitle)

  def texts = fragments.texts
  def examples = fragments.examples
}

/**
 * Create SpecStructures from header, arguments, fragments
 */
object SpecStructure {
  def apply(header: SpecHeader): SpecStructure =
    new SpecStructure(header, Arguments(), () => Fragments())

  def apply(header: SpecHeader, arguments: Arguments): SpecStructure =
    new SpecStructure(header, arguments, () => Fragments())

  def create(header: SpecHeader, fragments: =>Fragments): SpecStructure =
    new SpecStructure(header, Arguments(), () => fragments)

  def create(header: SpecHeader, arguments: Arguments, fragments: =>Fragments): SpecStructure =
    new SpecStructure(header, arguments, () => fragments)

  /** return true if s1 depends on s2, i.e, s1 has a link to s2 */
  val dependsOn = (s1: SpecStructure, s2: SpecStructure) => {
    val s1Links = s1.fragments.fragments.collect { case Fragment(l: SpecificationLink, _, _) => l.specClassName }
    s1Links.contains(s2.specClassName)
  }

  def empty(klass: Class[_]) =
    SpecStructure(SpecHeader(klass))

}
