package org.specs2
package specification
package create

import org.specs2.execute.{AsResult, Result}
import org.specs2.data.NamedTag
import specification.core._
import Execution._
import control.ImplicitParameters._
import specification.core.Text

/**
 * Interface for creating specification fragments
 */
trait FragmentFactory:
  def example(description: Description, execution: Execution): Fragment
  def example(text: String, execution: Execution): Fragment
  def example[T : AsResult](text: String, result: =>T): Fragment
  def example[T : AsResult](description: Description, result: =>T): Fragment
  def example[T : AsResult](text: String, withDescription: String => T): Fragment
  def example[T : AsResult](text: String, withDescriptionAndEnv: (String, Env) => T): Fragment
  def example[T](text: String, withEnv: Env => T)(using as: AsResult[T], p: ImplicitParam): Fragment

  def tag(names: String*): Fragment
  def taggedAs(names: String*): Fragment
  def section(names: String*): Fragment
  def asSection(names: String*): Fragment

  def mark(tag: NamedTag): Fragment
  def markAs(tag: NamedTag): Fragment
  def markSection(tag: NamedTag): Fragment
  def markSectionAs(tag: NamedTag): Fragment

  def action[T : AsExecution](t: =>T): Fragment
  def step[T : AsExecution](t: =>T): Fragment

  def text(t: String): Fragment
  def code(t: String): Fragment

  /** add a newline */
  def break: Fragment
  def start: Fragment
  def end: Fragment

  def tab: Fragment
  def tab(n: Int): Fragment
  def backtab: Fragment
  def backtab(n: Int): Fragment

  def link(link: SpecificationRef): Fragment
  def see(link: SpecificationRef): Fragment

/**
 * Default implementation of the FragmentFactory
 */
trait DefaultFragmentFactory extends FragmentFactory:
  def example(description: Description, execution: Execution): Fragment = Fragment(description, execution)
  def example(text: String, execution: Execution): Fragment = example(Text(text), execution)
  def example[T : AsResult](description: Description, r: =>T): Fragment    = Fragment(description, result(r))
  def example[T : AsResult](text: String, r: =>T): Fragment                = example(Description.text(text), r)
  def example[T : AsResult](text: String, withText: String => T): Fragment = example(text, withText(text))
  def example[T : AsResult](text: String, withDescriptionAndEnv: (String, Env) => T): Fragment =
    example(text, Execution.withEnv((env: Env) => withDescriptionAndEnv(text, env)))
  def example[T](text: String, withEnv: Env => T)(using as: AsResult[T], p: ImplicitParam): Fragment =
    example(text, Execution.withEnv(withEnv))

  def tag(names: String*): Fragment       = Fragment(Description.tag(names*), Execution.NoExecution)
  def taggedAs(names: String*): Fragment  = Fragment(Description.taggedAs(names*), Execution.NoExecution)
  def section(names: String*): Fragment   = Fragment(Description.section(names*), Execution.NoExecution)
  def asSection(names: String*): Fragment = Fragment(Description.asSection(names*), Execution.NoExecution)

  def mark(tag: NamedTag): Fragment          = Fragment(Description.mark(tag), Execution.NoExecution)
  def markAs(tag: NamedTag): Fragment        = Fragment(Description.markAs(tag), Execution.NoExecution)
  def markSection(tag: NamedTag): Fragment   = Fragment(Description.markSection(tag), Execution.NoExecution)
  def markSectionAs(tag: NamedTag): Fragment = Fragment(Description.markSectionAs(tag), Execution.NoExecution)

  def action[T : AsExecution](t: =>T): Fragment = Fragment(NoText, AsExecution[T].execute(t))
  def step[T : AsExecution](t: =>T): Fragment   = Fragment(NoText, AsExecution[T].execute(t).join)

  def text(t: String)           = Fragment(Text(t), Execution.NoExecution)
  def code(t: String)           = Fragment(Description.code(t), Execution.NoExecution)

  def break                     = Fragment(Br, Execution.NoExecution)
  def start                     = Fragment(Start, Execution.NoExecution)
  def end                       = Fragment(End, Execution.NoExecution)
  def tab: Fragment             = tab(1)
  def tab(n: Int): Fragment     = Fragment(Tab(n), Execution.NoExecution)
  def backtab: Fragment         = backtab(1)
  def backtab(n: Int): Fragment = Fragment(Backtab(n), Execution.NoExecution)

  def link(link: SpecificationRef)  = Fragment(link, Execution.specificationStats(link.specClassName))
  def see(link: SpecificationRef)   = Fragment(link, Execution.NoExecution)

/**
 * Trait for anything requiring a fragment factory
 */
trait FragmentsFactory:
  protected def fragmentFactory: FragmentFactory = DefaultFragmentFactory

object DefaultFragmentFactory extends DefaultFragmentFactory
