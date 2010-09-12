package org.specs2
package specification

sealed trait Fragment
case class SpecStart(name: String) extends Fragment
case class SpecEnd(name: String) extends Fragment
case class Text(t: String) extends Fragment
case class Group(fragments: List[Fragment])
case class Example(desc: String = "", body: Option[()=>Result] = None) extends Fragment { 
  def ^(a: Example) = Examples(List(this, a))
}

object end extends Fragment
object par extends Fragment
object br extends Fragment
