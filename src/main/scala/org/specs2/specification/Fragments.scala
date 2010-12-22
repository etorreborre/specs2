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
case class Fragments (fragments: Seq[Fragment]) {
  def add(e: =>Fragment) = copy(fragments = fragments :+ e)
  import StandardFragments._
  override def toString = fragments.mkString("\n")
  def ^(e: =>Fragment) = e match {
    case s @ SpecStart(n, a) => Fragments(s.withArgs(arguments) +: minusStart())
    case _                   => add(e)
  }
  def ^(g: Group) = copy(fragments = fragments ++ g.fragments)
  def ^(a: Arguments) = copy(fragments = start.withArgs(a) +: minusStart())

  def executables: Seq[Executable] = fragments.collect { case e: Executable => e }
  def overrideArgs(args: Arguments) = Fragments(start.overrideArgs(args) +: minusStart())

  def start = fragments.headOption match {
    case Some(s @ SpecStart(n, a)) => s
    case _                         => SpecStart("")
  }
  def arguments = start.arguments
  def end = fragments.lastOption match {
    case Some(e @ SpecEnd(n)) if (n.id == start.name.id) => e
    case _                    => SpecEnd("")
  }
  def minusStart(fs: Seq[Fragment] = fragments) = fs.headOption match {
    case Some(SpecStart(_, _)) => fs.drop(1)
    case _                     => fs
  }
  def minusEnd(fs: Seq[Fragment] = fragments) = fs.lastOption match {
    case Some(SpecEnd(_)) => fs.dropRight(1)
    case _                => fs
  }
  private def middle = minusEnd(minusStart())

}
case object Fragments {
  def create(fs: Fragment*) = new Fragments(fs)
  
  def isExample: Function[Fragment, Boolean] = { case Example(_, _) => true; case _ => false }
  def isStep: Function[Fragment, Boolean] = { case Step(_) => true; case _ => false }
  
  /** 
   * add a SpecStart and SpecEnd if there are none
   * 
   * Makes sure that the arguments instance in the Fragments object and in the SpecStart
   * fragment are the same 
   */
  def withSpecStartEnd(fragments: Fragments, name: SpecName): Fragments = {
    val specStart = fragments.start.withName(name)
    Fragments(specStart +: fragments.middle :+ fragments.end.withName(specStart.name))
  }
  def withSpecStartEnd(fragments: Fragments, s: SpecificationStructure): Fragments = withSpecStartEnd(fragments, SpecName(s))

}

