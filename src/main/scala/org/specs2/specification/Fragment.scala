package org.specs2
package specification
import execute._

sealed trait Fragment
case class SpecStart(name: String) extends Fragment
case class SpecEnd(name: String) extends Fragment
case class Group(fragments: List[Fragment])
case class Text(t: String) extends Fragment
case class Example(desc: String = "", body: () => Result) extends Fragment with Executable { 
  def ^(a: Fragment) = Fragments(List(this, a))
  def execute = body()
}
case class Step(action: () => Result) extends Fragment with Executable {
  def ^(a: Fragment) = Fragments(List(this, a))
  def execute = action()
}
object StandardFragments {
  case class End() extends Fragment
  case class Par() extends Fragment
  case class Br() extends Fragment
}

