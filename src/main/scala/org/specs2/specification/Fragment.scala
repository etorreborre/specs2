package org.specs2
package specification

import main.Arguments
import execute._
import control.LazyParameter

/**
 * A Fragment is a piece of a specification. It can be a piece of text, an action or 
 * an Example
 */
sealed trait Fragment {
  def matches(s: String) = true
}
case class SpecStart(name: String, arguments: Arguments = Arguments()) extends Fragment {
  override def matches(s: String) = name matches s
  override def toString = "SpecStart("+name+")"
}
case class SpecEnd(name: String) extends Fragment {
  override def matches(s: String) = name matches s
}
case class Group(fragments: Seq[Fragment])
case class Text(t: String) extends Fragment {
  override def matches(s: String) = t.matches(s)
}
case class Example private (desc: String = "", body: () => Result) extends Fragment with Executable { 
  def execute = body()
  override def matches(s: String) = desc.matches(s)
  override def toString = "Example("+desc+")"
}
case object Example {
  def apply[T <% Result](desc: String, body: LazyParameter[T]) = new Example(desc, () => body.value)
}
case class Step private (action: () => Result) extends Fragment with Executable {
  def execute = action()
  override def toString = "Step"
}
case object Step {
  def apply(action: LazyParameter[Result]) = new Step(() => action.value)
}

/**
 * Those standard Fragments are used to format the specification text:
 *  * End() can be used to "reset" the indentation of text  
 *  * Br() can be used to insert a newline  
 *  * Par() can be used to insert 2 newlines  
 */
private[specs2]
object StandardFragments {
  case class End() extends Fragment
  case class Par() extends Fragment
  case class Br() extends Fragment
  case class Tab() extends Fragment
  case class Backtab() extends Fragment
}

