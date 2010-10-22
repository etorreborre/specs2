package org.specs2
package reporter
import specification._
import io._
import org.scalacheck._

class SelectionSpec extends SpecificationWithJUnit with ScalaCheck with ArbitraryFragments { def is =
                                                                                          """
  Before executing and reporting a specification, the fragments must be selected and 
  sorted:
  
  * they must be selected to keep only the relevant ones
  * they must be sorted to respect their execution dependencies
    * steps must be executed before examples as specified
    * tagged examples with dependencies must respect their specified ordering
                                                                                          """^
  "If a specification contains steps they must be grouped before the examples"            ^
  "  2 consecutive steps must be in the same list"                                        ! steps().e1^
  "  2 consecutive examples must be in the same list"                                     ! steps().e2^
  "  an example followed by a steps must not be in the same list"                         ! steps().e3^
  "  a step followed by an example must not be in the same list"                          ! steps().e4^
  "  so that steps and examples are always separate"                                      ! steps().e5^
                                                                                          end
  
  case class steps()  {
    def e1 = Prop.forAll { (fs: Fragments) =>
      val selected = select(fs ^ action("1"))
      val selected2 = select(fs ^ action("1") ^ action("2"))
      selected2.size == selected.size
    }           
    def e2 = Prop.forAll { (fs: Fragments) =>
      val selected = select(fs ^ ex1)
      val selected2 = select(fs ^ ex1 ^ ex2)
      selected2.size == selected.size
    }           
    def e3 = Prop.forAll { (fs: Fragments) =>
      val selected = select(fs ^ ex1)
      val selected2 = select(fs ^ ex1 ^ action("1"))
      selected2.size == selected.size + 1
    }           
    def e4 = Prop.forAll { (fs: Fragments) =>
      val selected = select(fs ^ action("1"))
      val selected2 = select(fs ^ action("1") ^ ex2)
      selected2.size == selected.size + 1
    }           
    def e5 = {
      val fragments = "intro" ^ action("1") ^ ex1 ^ ex2 ^ action("2") ^ action("3") ^ ex1 ^ ex2
      select(fragments).map(l => l.map(_.toString)).mkString("\n") must_== List(
      "List(Text(intro), Step)",
      "List(Example(ex1), Example(ex2))",
      "List(Step, Step)",
      "List(Example(ex1), Example(ex2))").mkString("\n")  
    }
    val ex1 = "ex1" ! success
    val ex2 = "ex2" ! success
    val selection = new Selection with MockOutput
    def action(message: String) = Action(selection.println(message)) 
    def select(f: Fragments) = {
      selection.select(new Specification { def is = f }.content).map(l => l.map(_.toString))
    }
  }

}
