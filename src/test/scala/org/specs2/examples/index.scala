package examples

import org.specs2._
import runner.FilesRunner._

class index extends Specification { def is =

  examplesLinks("Example specifications")

  def examplesLinks(t: String) = {
    specifications("**/examples/*.scala").
      foldLeft(t.title) { (res, cur) => res ^ see(cur) }
  }
}