package org.specs2
package specification

import main.Arguments
import execute._
import text.Trim._
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
case class Text(t: String) extends Fragment {
  override def matches(s: String) = t.matches(s)
}
case class Group(fragments: Seq[Fragment])
case class Example private[specification] (desc: String = "", body: () => Result) extends Fragment with Executable { 
  def execute = body()
  override def matches(s: String) = desc.removeAll("\\n").matches(s)
  override def toString = "Example("+desc+")"
  override def equals(a: Any) = {
    a match {
      case e: Example => desc equals e.desc 
      case _          => false
    }
  }
}
case object Example {
  def apply[T <% Result](desc: String, body: =>T) = new Example(desc, () => body)
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
  case class Tab(n: Int = 1) extends Fragment
  case class Backtab(n: Int = 1) extends Fragment
}

