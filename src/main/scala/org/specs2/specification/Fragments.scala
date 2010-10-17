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
case class Fragments(private val fragmentList: () => List[Fragment]) {
  def fragments = fragmentList()
  import StandardFragments._
  override def toString = fragments.mkString("\n")
  def ^(e: =>Fragment) = copy(fragmentList = () => this.fragments :+ e)
  def ^(e: Group) = copy(fragmentList = () => this.fragments ++ e.fragments) 
  def Fragments: List[Example] = fragments.collect { case ex: Example => ex }
  def executables: List[Executable] = fragments.collect { case e: Executable => e }
}
case object Fragments {
  def apply(fragments: LazyParameter[Fragment]*) = new Fragments(() => fragments.map(_.getValue).toList)
}

