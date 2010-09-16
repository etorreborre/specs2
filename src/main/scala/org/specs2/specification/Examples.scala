package org.specs2
package specification

case class Examples(fragments: List[Fragment]) {
  import PredefinedFragments._
  override def toString = fragments.mkString("\n")
  def ^(e: Fragment) = copy(fragments = this.fragments :+ e)
  def ^(e: Group) = copy(fragments = this.fragments ++ e.fragments) 
  def ^^(examples: Examples) = copy(fragments = (this.fragments :+ end :+ p) ++ examples.fragments)
  def examples: List[Example] = fragments.collect { case ex: Example => ex }
  def executables: List[Executable] = fragments.collect { case e: Executable => e }
}
