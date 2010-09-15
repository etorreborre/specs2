package org.specs2
package specification
import execute._

sealed trait Fragment
case class SpecStart(name: String) extends Fragment
case class SpecEnd(name: String) extends Fragment
case class Text(t: String) extends Fragment
case class Group(fragments: List[Fragment])
case class Example(desc: String = "", body: () => Result) extends Fragment with Executable { 
  def ^(a: Fragment) = Examples(List(this, a))
  def execute = body()
}
case class Step(action: () => Result) extends Fragment with Executable {
  def ^(a: Fragment) = Examples(List(this, a))
  def execute = action()
}
trait Executable {
  def execute: Result
}
trait PredefinedFragments {
  object end extends Fragment
  object par extends Fragment
  object br extends Fragment
}

