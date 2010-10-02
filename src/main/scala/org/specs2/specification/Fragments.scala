package org.specs2
package specification
import execute.Executable
import scalaz._
/**
 * An Fragments object is a list of fragments which can be related 
 * to other fragments by using the ^ method
 */
case class Fragments(fragments: List[Fragment]) {
  import StandardFragments._
  override def toString = fragments.mkString("\n")
  def ^(e: Fragment) = copy(fragments = this.fragments :+ e)
  def ^(e: Group) = copy(fragments = this.fragments ++ e.fragments) 
  def Fragments: List[Example] = fragments.collect { case ex: Example => ex }
  def executables: List[Executable] = fragments.collect { case e: Executable => e }
}
case object Fragments {
  def apply(fragments: Fragment*) = new Fragments(fragments.toList)
}

