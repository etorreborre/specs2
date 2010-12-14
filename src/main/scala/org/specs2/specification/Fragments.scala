package org.specs2
package specification

import execute.Executable
import control.LazyParameters._
import control.LazyParameter
import main.Arguments
import StandardFragments._
/**
 * A Fragments object is a list of fragments which can be related 
 * to other fragments by using the ^ method
 * 
 * A Fragments object carries an Arguments instance containing options for selecting,
 * executing and reporting Fragments
 */
case class Fragments private (private val fragmentList: () => Seq[Fragment], arguments: Arguments = Arguments()) {
  def fragments = fragmentList()
  def add(e: =>Fragment) = copy(fragmentList = () => this.fragments :+ e) 
  import StandardFragments._
  override def toString = fragments.mkString("\n")
  def ^(e: =>Fragment) = add(e)
  def ^(g: Group) = copy(fragmentList = () => this.fragments ++ g.fragments)
  def ^(a: Arguments) = copy(fragmentList = () => this.fragments, arguments = a)

  def executables: Seq[Executable] = fragments.collect { case e: Executable => e }
}
case object Fragments {
  def apply(fragments: LazyParameter[Fragment]*) = new Fragments(() => fragments.map(_.value))
  def apply(fragments: Seq[Fragment])(implicit args: Arguments) = new Fragments(() => fragments, args)
  
  def isExample: Function[Fragment, Boolean] = { case Example(_, _) => true; case _ => false }
  def isStep: Function[Fragment, Boolean] = { case Step(_) => true; case _ => false }
  
  /** 
   * add a SpecStart and SpecEnd if there are none
   * 
   * Makes sure that the arguments instance in the Fragments object and in the SpecStart
   * fragment are the same 
   */
  def withSpecStartEnd(fragments: Fragments, name: SpecName): Fragments = {
    val withStartFragments = fragments.fragments.headOption match {
      case Some(SpecStart(n, _)) => SpecStart(n, fragments.arguments) +: fragments.fragments.drop(1)
      case other => SpecStart(name, fragments.arguments) +: (Par() +: fragments.fragments)
    }
    val withStartAndEndFragments = withStartFragments.lastOption match {
      case Some(SpecEnd(n)) => withStartFragments
      case other => withStartFragments :+ SpecEnd(name)
    }
    Fragments(withStartAndEndFragments)(fragments.arguments)
  }
  def withSpecStartEnd(fragments: Fragments, name: String): Fragments = withSpecStartEnd(fragments, SpecName(name))

}

