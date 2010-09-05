package org.specs2

class specificationSpec extends Specification { 
  val spec =
    "in this context"^
      "do this" ~ e1^
      "do that" ~ e2^
    "in there"^
      "do also this" ~ e1^
      "do also that" ~ e2
  def e1 = List()
  def e2 = List()

}
object test extends specificationSpec with Application { 
  override def main(args: Array[String]) = {
	println(spec.getClass+":\n"+spec)  
	println(spec.toString)  
  }	
}
