package org.specs2
package specification
import execute.Executable
import scalaz._
import control.LazyParameters._
import control.LazyParameter
/**
 * An Fragments object is a list of fragments which can be related 
 * to other fragments by using the ^ method
 */
case class Fragments(private val fragmentList: () => List[Fragment], arguments: Args = Args()) {
  def fragments = fragmentList()
  import StandardFragments._
  override def toString = fragments.mkString("\n")
  def ^(e: =>Fragment) = copy(fragmentList = () => this.fragments :+ e)
  def ^(e: Group) = copy(fragmentList = () => this.fragments ++ e.fragments) 
  def Fragments: List[Example] = fragments.collect { case ex: Example => ex }
  def executables: List[Executable] = fragments.collect { case e: Executable => e }
  def ^(a: Args) = copy(fragmentList = () => this.fragments, arguments = a)
}
case object Fragments {
  def apply(fragments: LazyParameter[Fragment]*) = new Fragments(() => fragments.map(_.value).toList)
  def isExample: Function[Fragment, Boolean] = { case Example(_, _) => true; case _ => false }
  def isStep: Function[Fragment, Boolean] = { case Step(_) => true; case _ => false }
}

