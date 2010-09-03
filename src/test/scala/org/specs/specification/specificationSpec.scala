package org.specs

class specificationSpec extends Specification { 
  val spec =
  "it should"^
    "and it should"^
      "do this" ~ e1^
      "do that" ~ e2^
    "but also it should blah bli bal"^
      "do this" ~ d().e3^
      "do that" ~ d().e4^
  "and eventually it should"^
      "do this" ~ c.e1()
  
  
  def e1 = List()
  def e2 = List()
  class b() {
    val e0 = ""
    val before = "do before"
  }
  case object c extends b() {
	val e1 = () => List(e0)
	val e2 = () => List(e0)
  }
  case class d() extends b {
	def e3 = List(e0)
	def e4 = List(e0)
  }
}
object test extends specificationSpec with Application { 
  override def main(args: Array[String]) = {
	println(spec)  
  }	
}