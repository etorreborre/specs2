package examples

import org.specs2._
import runner.SpecificationsFinder._

class index extends Specification { def is =

  examplesLinks("Example specifications")

  // see the SpecificationsFinder trait for the parameters of the 'specifications' method
  def examplesLinks(t: String) = specifications().foldLeft(t.title) { (res, cur) => res ^ see(cur) }
  
}