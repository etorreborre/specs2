package org.specs2
package specification

case class Examples(fragments: List[Fragment]) {
  override def toString = fragments.mkString("\n")
  def ^(e: Fragment) = copy(fragments = this.fragments :+ e) 
  def ^(e: Group) = copy(fragments = this.fragments ++ e.fragments) 
  def ^^(examples: Examples) = copy(fragments = (this.fragments :+ end :+ par) ++ examples.fragments)
}
case class Example(desc: String = "", body: Option[()=>Result] = None) extends Fragment { 
  def ^(a: Example) = Examples(List(this))
}
