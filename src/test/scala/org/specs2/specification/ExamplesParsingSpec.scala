package org.specs2.specification
import org.specs2._

class ExamplesParsingSpec extends Specification {
  val examples = 
  "A list of fragments can be parsed as a Tree"^ 
  "A Text fragment will be parsed as a tree containing a single node"~e1
  
  def e1 = parse(Text("hello")) must_== Tree(Text("hello"))

  def parse(examples: Examples): Traversable[Fragment] = new TreeExamplesParser{}.parse(examples)
  def parse(fragment: Fragment): Traversable[Fragment] = parse(Examples(List(fragment)))
}