package org.specs2
package specification

import execute.Executable
import control.LazyParameters._
import control.LazyParameter
import main.Arguments
/**
 * A Fragments object is a list of fragments which can be related 
 * to other fragments by using the ^ method
 */
case class Fragments private (private val fragmentList: () => Seq[Fragment], arguments: Arguments = Arguments()) {
  def fragments = fragmentList()
  import StandardFragments._
  override def toString = fragments.mkString("\n")
  def ^(e: =>Fragment) = copy(fragmentList = () => this.fragments :+ e)
  def ^(e: Group) = copy(fragmentList = () => this.fragments ++ e.fragments) 
  def Fragments: Seq[Example] = fragments.collect { case ex: Example => ex }
  def executables: Seq[Executable] = fragments.collect { case e: Executable => e }
  def ^(a: Arguments) = copy(fragmentList = () => this.fragments, arguments = a)
}
case object Fragments {
  def apply(fragments: LazyParameter[Fragment]*) = new Fragments(() => fragments.map(_.value))
  def apply(fragments: Seq[Fragment])(implicit args: Arguments) = new Fragments(() => fragments, args)
  
  def isExample: Function[Fragment, Boolean] = { case Example(_, _) => true; case _ => false }
  def isStep: Function[Fragment, Boolean] = { case Step(_) => true; case _ => false }
}

