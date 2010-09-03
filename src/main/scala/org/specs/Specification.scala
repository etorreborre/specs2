package org.specs

trait Specification {
  class Examples(val e: Example*) {
	override def toString = e.mkString("\n")
  }
  case class Example(s: String = "", private val _nested: List[Example] = Nil, parent: Option[Example] = None, body: Option[Any] = None) { 
    def level: Int = parent match { case None => 0; case Some(p) => p.level + 1}
    def nested = _nested.map(_.copy(parent=Some(this)))
    def add(a: Example) = copy(_nested = this._nested :+ a)
    def getOrMakeParent = parent.getOrElse(Example(_nested=List(this)))
	def ~[T](t: =>T) = this.copy(body = Some(t))
	def ^(a: Example) = {
	  this.body match {
	 	case None => a.body match {
	 	  case Some(ab) => add(a)
	 	  case None => getOrMakeParent.add(a); a
	 	}
	 	case Some(b) => a.body match {
	 	  case Some(ab) => getOrMakeParent.add(a); a
	 	  case None => getOrMakeParent.getOrMakeParent.add(a); a
	 	}
	  }	
	}
    def indent = "  "*(level+1)
	def show: String = "("+s + (if (nested.isEmpty) "" else nested.map(_.show).mkString("\n"+indent, "\n"+indent, "\n"))+")"
  }
  implicit def forExample(s: String): Example = new Example(s)
  class ExampleDesc(s: String) {
	def ^(a: Example) = new Example(s).add(a)
	def ~[T](t: =>T) = new Example(s, body = Some(t))
  }
}