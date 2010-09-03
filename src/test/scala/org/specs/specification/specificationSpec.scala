package org.specs

class specificationSpec extends Specification { 
  val spec =
//  "it should"^
    "in this context"^
      "do this" ~ e1^
      "do that" ~ e2^
    "in there"^
      "do also this" ~ e1^
      "do also that" ~ e2

    //    "in that context"^
//      "do this other " ~ e1^
//      "do that other" ~ e2|
//  "it should also"^
//    "do it again" ~ e1^
//      "and again" ~ e1
  
  def e1 = List()
  def e2 = List()

}
object test extends specificationSpec with Application { 
  override def main(args: Array[String]) = {
	println(spec.getClass+":\n"+spec)  
	println(spec.show)  
  }	
}

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

//  "it should" ^
//    "do this" ~ e1^
//    "do that" ~ e2|
//  "it should also"^
//    "do it again" ~ e1^
//      "and again" ~ e1
//      "do that" ~ e2^
//    "but also it should blah bli bal"^
//      "do this" ~ d().e3^
//      "do that" ~ d().e4^
//  "and eventually it should"^
//      "do this" ~ c.e1()
