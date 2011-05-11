import org.specs2._
import runner.FilesRunner

class index extends Specification with FilesRunner { def is =

  examplesLinks("Example specifications")

  def examplesLinks(t: String) = {
    specificationNames("src/test/scala", "**/examples/*.scala", ".*Spec").
      flatMap(n => createSpecification(n)).
      foldLeft(t.title) { (res, cur) => res ^ link(cur) }
  }
}