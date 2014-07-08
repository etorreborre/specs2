package org.specs2
package specification
package create

import org.specs2.execute.{Result, Success, AsResult}
import org.specs2.data.{NamedTag, Tag}
import specification.core._
import Description._
import Execution._
import control.ImplicitParameters._
import specification.core.Text

/**
 * Interface for creating specification fragments
 */
trait FragmentFactory {
  def example(description: Description, execution: Execution): Fragment
  def example[T : AsResult](description: String, result: =>T): Fragment
  def example[T : AsResult](description: Description, result: =>T): Fragment
  def example[T : AsResult](s: String, withDescription: String => T): Fragment
  def example[T : AsResult](s: String, withDescriptionAndEnv: (String, Env) => T): Fragment
  def example[T](text: String, withEnv: Env => T)(implicit as: AsResult[T], p: ImplicitParam): Fragment

  def tag(names: String*): Fragment
  def taggedAs(names: String*): Fragment
  def section(names: String*): Fragment
  def asSection(names: String*): Fragment

  def mark(tag: NamedTag): Fragment
  def markAs(tag: NamedTag): Fragment
  def markSection(tag: NamedTag): Fragment
  def markSectionAs(tag: NamedTag): Fragment

  def action[T](t: =>T): Fragment
  def step[T](t: =>T): Fragment

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

  def link(link: SpecificationLink): Fragment
  def see(link: SpecificationLink): Fragment
}

/**
 * Default implementation of the FragmentFactory
 */
trait DefaultFragmentFactory extends FragmentFactory {
  def example(description: Description, execution: Execution): Fragment    = Fragment(description, execution)
  def example[T : AsResult](description: Description, r: =>T): Fragment    = Fragment(description, result(r))
  def example[T : AsResult](text: String, r: =>T): Fragment                = example(Description.text(text), r)
  def example[T : AsResult](text: String, withText: String => T): Fragment = example(text, withText(text))
  def example[T : AsResult](text: String, withDescriptionAndEnv: (String, Env) => T): Fragment =
    Fragment(Text(text), Execution.withEnv((env: Env) => withDescriptionAndEnv(text, env)))
  def example[T](text: String, withEnv: Env => T)(implicit as: AsResult[T], p: ImplicitParam): Fragment =
    Fragment(Text(text), Execution.withEnv(withEnv))

  def tag(names: String*): Fragment       = Fragment(Description.tag(names:_*), Execution.NoExecution)
  def taggedAs(names: String*): Fragment  = Fragment(Description.taggedAs(names:_*), Execution.NoExecution)
  def section(names: String*): Fragment   = Fragment(Description.section(names:_*), Execution.NoExecution)
  def asSection(names: String*): Fragment = Fragment(Description.asSection(names:_*), Execution.NoExecution)

  def mark(tag: NamedTag): Fragment          = Fragment(Description.mark(tag), Execution.NoExecution)
  def markAs(tag: NamedTag): Fragment        = Fragment(Description.markAs(tag), Execution.NoExecution)
  def markSection(tag: NamedTag): Fragment   = Fragment(Description.markSection(tag), Execution.NoExecution)
  def markSectionAs(tag: NamedTag): Fragment = Fragment(Description.markSectionAs(tag), Execution.NoExecution)

  def action[T](t: =>T): Fragment = Fragment(NoText, result { Result.resultOrSuccess(t) })
  def step[T](t: =>T): Fragment   = Fragment(NoText, result { Result.resultOrSuccess(t) }.join)

  def text(t: String)           = Fragment(Text(t), Execution.NoExecution)
  def code(t: String)           = Fragment(Description.code(t), Execution.NoExecution)

  def break                     = Fragment(Br, Execution.NoExecution)
  def start                     = Fragment(Start, Execution.NoExecution)
  def end                       = Fragment(End, Execution.NoExecution)
  def tab: Fragment             = tab(1)
  def tab(n: Int): Fragment     = Fragment(Tab(n), Execution.NoExecution)
  def backtab: Fragment         = backtab(1)
  def backtab(n: Int): Fragment = Fragment(Backtab(n), Execution.NoExecution)

  def link(link: SpecificationLink)  = Fragment(link, Execution.SpecificationStats(link.specClassName))
  def see(link: SpecificationLink)   = Fragment(link, Execution.NoExecution)

}

import execute.{Result, AsResult}
import control.ImplicitParameters._

/**
 * Fragment factory that is creating examples with a given context
 * and delegating to another factory all the rest
 */
class ContextualFragmentFactory(factory: FragmentFactory, context: Env => Context) extends FragmentFactory {
  def example(description: Description, execution: Execution): Fragment =
    factory.example(description, execution.copy(run = execution.run.map(f => (e: Env) => context(e)(f(e)))))

  def example[T : AsResult](description: String, result: =>T): Fragment =
    factory.example(description, (env: Env) => context(env)(result))

  def example[T : AsResult](description: Description, result: =>T): Fragment =
    factory.example(description, Execution.withEnv((env: Env) => context(env)(result)))

  def example[T : AsResult](s: String, withDescription: String => T): Fragment =
    factory.example(s, (s: String, e: Env) => context(e)(withDescription(s)))

  def example[T : AsResult](s: String, withDescriptionAndEnv: (String, Env) => T): Fragment =
    factory.example(s, (s: String, e: Env) => context(e)(withDescriptionAndEnv(s, e)))

  def example[T](text: String, withEnv: Env => T)(implicit as: AsResult[T], p: ImplicitParam): Fragment =
  factory.example(text, (e: Env) => context(e)(withEnv(e)))(Result.resultAsResult, p)

  def tag(names: String*): Fragment                = factory.tag(names:_*)
  def taggedAs(names: String*): Fragment           = factory.taggedAs(names:_*)
  def section(names: String*): Fragment            = factory.section(names:_*)
  def asSection(names: String*): Fragment          = factory.asSection(names:_*)

  def mark(tag: NamedTag): Fragment                = factory.mark(tag)
  def markAs(tag: NamedTag): Fragment              = factory.markAs(tag)
  def markSection(tag: NamedTag): Fragment         = factory.markSection(tag)
  def markSectionAs(tag: NamedTag): Fragment       = factory.markSectionAs(tag)

  def action[T](t: =>T): Fragment                  = factory.action[T](t)
  def step[T](t: =>T): Fragment                    = factory.step[T](t)
  def text(t: String): Fragment                    = factory.text(t)
  def code(t: String): Fragment                    = factory.code(t)
  def break: Fragment                              = factory.break
  def start: Fragment                              = factory.start
  def end: Fragment                                = factory.end
  def tab: Fragment                                = factory.tab
  def tab(n: Int): Fragment                        = factory.tab(n)
  def backtab: Fragment                            = factory.backtab
  def backtab(n: Int): Fragment                    = factory.backtab(n)
  def link(link: SpecificationLink): Fragment      = factory.link(link)
  def see(link: SpecificationLink): Fragment       = factory.see(link)

}

/**
 * FragmentFactory trait which can be mixed in a Specification to create
 * Fragments but which will delegate the creation to a factory member
 */
trait DelegatedFragmentFactory extends FragmentsFactory with FragmentFactory {
  private val factory = fragmentFactory

  def example(description: Description, execution: Execution): Fragment = factory.example(description, execution)
  def example[T : AsResult](description: String, result: =>T): Fragment = factory.example(description, result)
  def example[T : AsResult](description: Description, result: =>T): Fragment = factory.example(description, result)
  def example[T : AsResult](s: String, withDescription: String => T): Fragment = factory.example(s, withDescription)
  def example[T : AsResult](text: String, withDescriptionAndEnv: (String, Env) => T): Fragment = factory.example(text, withDescriptionAndEnv)
  def example[T](text: String, withEnv: Env => T)(implicit as: AsResult[T], p: ImplicitParam): Fragment = factory.example(text, withEnv)(as, p)

  def tag(names: String*): Fragment             = factory.tag(names:_*)
  def taggedAs(names: String*): Fragment        = factory.taggedAs(names:_*)
  def section(names: String*): Fragment         = factory.section(names:_*)
  def asSection(names: String*): Fragment       = factory.asSection(names:_*)

  def mark(tag: NamedTag): Fragment             = factory.mark(tag)
  def markAs(tag: NamedTag): Fragment           = factory.markAs(tag)
  def markSection(tag: NamedTag): Fragment      = factory.markSection(tag)
  def markSectionAs(tag: NamedTag): Fragment    = factory.markSectionAs(tag)

  def action[T](t: =>T): Fragment               = factory.action[T](t)
  def step[T](t: =>T): Fragment                 = factory.step[T](t)
  def text(t: String): Fragment                 = factory.text(t)
  def code(t: String): Fragment                 = factory.code(t)
  def break: Fragment                           = factory.break
  def start: Fragment                           = factory.start
  def end: Fragment                             = factory.end
  def tab: Fragment                             = factory.tab
  def tab(n: Int): Fragment                     = factory.tab(n)
  def backtab: Fragment                         = factory.backtab
  def backtab(n: Int): Fragment                 = factory.backtab(n)
  def link(link: SpecificationLink): Fragment   = factory.link(link)
  def see(link: SpecificationLink)              = factory.see(link)

}

/**
 * Trait for anything requiring a fragment factory
 */
trait FragmentsFactory {
  protected def fragmentFactory: FragmentFactory = DefaultFragmentFactory
}

object DefaultFragmentFactory extends DefaultFragmentFactory


