package org.specs2
package specification
package create

import execute.{Success, AsResult}
import org.specs2.data.{NamedTag, Tag}
import specification.core._
import Description._
import Execution._
import control.ImplicitParameters._
import specification.core.RawText

/**
 * Interface for creating specification fragments
 */
trait FragmentFactory {
  def Example(description: Description, execution: Execution): Fragment
  def Example[T : AsResult](description: String, result: =>T): Fragment
  def Example[T : AsResult](description: Description, result: =>T): Fragment
  def Example[T : AsResult](s: String, withDescription: String => T): Fragment
  def Example[T](text: String, withEnv: Env => T)(implicit as: AsResult[T], p: ImplicitParam): Fragment

  def Tag(names: String*): Fragment
  def TaggedAs(names: String*): Fragment
  def Section(names: String*): Fragment
  def AsSection(names: String*): Fragment

  def Mark(tag: NamedTag): Fragment
  def MarkAs(tag: NamedTag): Fragment
  def MarkSection(tag: NamedTag): Fragment
  def MarkSectionAs(tag: NamedTag): Fragment

  def Action[T](t: =>T): Fragment
  def Step[T](t: =>T): Fragment

  def Text(t: String): Fragment

  /** add a newline */
  def Break: Fragment
  def Start: Fragment
  def End: Fragment

  def Tab: Fragment
  def Tab(n: Int): Fragment
  def Backtab: Fragment
  def Backtab(n: Int): Fragment

  def Link(link: SpecificationLink): Fragment
}

/**
 * Default implementation of the FragmentFactory
 */
trait DefaultFragmentFactory extends FragmentFactory {
  def Example(description: Description, execution: Execution): Fragment   = Fragment(description, execution)
  def Example[T : AsResult](description: Description, r: =>T): Fragment   = Fragment(description, result(r))
  def Example[T : AsResult](text: String, r: =>T): Fragment               = Example(Description.text(text), r)
  def Example[T : AsResult](text: String, withText: String =>T): Fragment = Example(text, withText(text))
  def Example[T](text: String, withEnv: Env => T)(implicit as: AsResult[T], p: ImplicitParam): Fragment =
    Fragment(RawText(text), Execution.withEnv(withEnv))

  def Tag(names: String*): Fragment       = Fragment(tag(names:_*), Execution.NoExecution)
  def TaggedAs(names: String*): Fragment  = Fragment(taggedAs(names:_*), Execution.NoExecution)
  def Section(names: String*): Fragment   = Fragment(section(names:_*), Execution.NoExecution)
  def AsSection(names: String*): Fragment = Fragment(asSection(names:_*), Execution.NoExecution)

  def Mark(tag: NamedTag): Fragment          = Fragment(mark(tag), Execution.NoExecution)
  def MarkAs(tag: NamedTag): Fragment        = Fragment(markAs(tag), Execution.NoExecution)
  def MarkSection(tag: NamedTag): Fragment   = Fragment(markSection(tag), Execution.NoExecution)
  def MarkSectionAs(tag: NamedTag): Fragment = Fragment(markSectionAs(tag), Execution.NoExecution)

  def Action[T](t: =>T): Fragment = Fragment(NoText, result({ t; Success() }))
  def Step[T](t: =>T): Fragment   = Fragment(NoText, result({ t; Success() }).join)

  def Text(t: String)           = Fragment(text(t), Execution.NoExecution)

  def Break                     = Fragment(br, Execution.NoExecution)
  def Start                     = Fragment(start, Execution.NoExecution)
  def End                       = Fragment(end, Execution.NoExecution)
  def Tab: Fragment             = Tab(1)
  def Tab(n: Int): Fragment     = Fragment(tab(n), Execution.NoExecution)
  def Backtab: Fragment         = Backtab(1)
  def Backtab(n: Int): Fragment = Fragment(backtab(n), Execution.NoExecution)

  def Link(link: SpecificationLink) = Fragment(link, Execution.SpecificationStats(link.specClassName))

}

import execute.{Result, AsResult}
import control.ImplicitParameters._

/**
 * Fragment factory that is creating examples with a given context
 * and delegating to another factory all the rest
 */
class ContextualFragmentFactory(factory: FragmentFactory, context: Context) extends FragmentFactory {
  def Example(description: Description, execution: Execution): Fragment =
    factory.Example(description, execution.copy(run = execution.run.map(f => (e: Env) => context(f(e)))))

  def Example[T : AsResult](description: String, result: =>T): Fragment =
    factory.Example(description, context(result))

  def Example[T : AsResult](description: Description, result: =>T): Fragment =
    factory.Example(description, context(result))

  def Example[T : AsResult](s: String, withDescription: String => T): Fragment =
    factory.Example(s, (s: String) => context(withDescription(s)))

  def Example[T](text: String, withEnv: Env => T)(implicit as: AsResult[T], p: ImplicitParam): Fragment =
    factory.Example(text, (e: Env) => context(withEnv(e)))(Result.resultAsResult, p)

  def Tag(names: String*): Fragment             = factory.Tag(names:_*)
  def TaggedAs(names: String*): Fragment        = factory.TaggedAs(names:_*)
  def Section(names: String*): Fragment         = factory.Section(names:_*)
  def AsSection(names: String*): Fragment       = factory.AsSection(names:_*)

  def Mark(tag: NamedTag): Fragment                  = factory.Mark(tag)
  def MarkAs(tag: NamedTag): Fragment                = factory.MarkAs(tag)
  def MarkSection(tag: NamedTag): Fragment           = factory.MarkSection(tag)
  def MarkSectionAs(tag: NamedTag): Fragment         = factory.MarkSectionAs(tag)

  def Action[T](t: =>T): Fragment               = factory.Action[T](t)
  def Step[T](t: =>T): Fragment                 = factory.Step[T](t)
  def Text(t: String): Fragment                 = factory.Text(t)
  def Break: Fragment                           = factory.Break
  def Start: Fragment                           = factory.Start
  def End: Fragment                             = factory.End
  def Tab: Fragment                             = factory.Tab
  def Tab(n: Int): Fragment                     = factory.Tab(n)
  def Backtab: Fragment                         = factory.Backtab
  def Backtab(n: Int): Fragment                 = factory.Backtab(n)
  def Link(link: SpecificationLink): Fragment   = factory.Link(link)

}

/**
 * FragmentFactory trait which can be mixed in a Specification to create
 * Fragments but which will delegate the creation to a factory member
 */
trait DelegatedFragmentFactory extends FragmentsFactory with FragmentFactory {
  private val factory = fragmentFactory

  def Example(description: Description, execution: Execution): Fragment = factory.Example(description, execution)
  def Example[T : AsResult](description: String, result: =>T): Fragment = factory.Example(description, result)
  def Example[T : AsResult](description: Description, result: =>T): Fragment = factory.Example(description, result)
  def Example[T : AsResult](s: String, withDescription: String => T): Fragment = factory.Example(s, withDescription)
  def Example[T](text: String, withEnv: Env => T)(implicit as: AsResult[T], p: ImplicitParam): Fragment = factory.Example(text, withEnv)(as, p)

  def Tag(names: String*): Fragment             = factory.Tag(names:_*)
  def TaggedAs(names: String*): Fragment        = factory.TaggedAs(names:_*)
  def Section(names: String*): Fragment         = factory.Section(names:_*)
  def AsSection(names: String*): Fragment       = factory.AsSection(names:_*)

  def Mark(tag: NamedTag): Fragment                  = factory.Mark(tag)
  def MarkAs(tag: NamedTag): Fragment                = factory.MarkAs(tag)
  def MarkSection(tag: NamedTag): Fragment           = factory.MarkSection(tag)
  def MarkSectionAs(tag: NamedTag): Fragment         = factory.MarkSectionAs(tag)

  def Action[T](t: =>T): Fragment             = factory.Action[T](t)
  def Step[T](t: =>T): Fragment               = factory.Step[T](t)
  def Text(t: String): Fragment               = factory.Text(t)
  def Break: Fragment                         = factory.Break
  def Start: Fragment                         = factory.Start
  def End: Fragment                           = factory.End
  def Tab: Fragment                           = factory.Tab
  def Tab(n: Int): Fragment                   = factory.Tab(n)
  def Backtab: Fragment                       = factory.Backtab
  def Backtab(n: Int): Fragment               = factory.Backtab(n)
  def Link(link: SpecificationLink): Fragment = factory.Link(link)

}

/**
 * Trait for anything requiring a fragment factory
 */
trait FragmentsFactory {
  protected def fragmentFactory: FragmentFactory = DefaultFragmentFactory
}

object DefaultFragmentFactory extends DefaultFragmentFactory


