package org.specs2
package reporter
import specification._
import execute._
import io._
import org.specs2.mock._
import main._

class HtmlFileLinesSpec extends Specification with Mockito { def is =

  "A linked specification"                                                                                              ^
    "must create a new file"                                                                                            ! included().e1^
    "must get an icon representing its status"                                                                          ^
      "success if everything succeeds"                                                                                  ! included().e2^
      "failure if there is a failure"                                                                                   ! included().e3^
                                                                                                                        endp^
  "A see specification"                                                                                                 ^
    "must not create a new file"                                                                                        ! seeIt().e1^
    "must get an icon representing its status"                                                                          ^
      "success if everything succeeds"                                                                                  ! seeIt().e2^
      "failure if there is a failure"                                                                                   ! seeIt().e3^
                                                                                                                        end

  implicit val argument = args()

  trait LinkedSpecifications extends MockHtmlPrinter {
    val successfulSubSpec = new Specification { def is = "ex1" ! success }
    val failedSubSpec     = new Specification { def is = "failedSubSpec".title ^ "ex1" ! failure }
  }

  case class included() extends LinkedSpecifications {
    val spec1: Fragments = "ex1" ! failure ^ "a " ~ ("successfull spec", successfulSubSpec) ^ end
    val spec2: Fragments = "ex1" ! success ^ "a " ~ ("failed spec", failedSubSpec) ^ end

    def e1 = htmlLines(spec1) must have size(7)
    def e2 = print(spec1) must \\("img", "src" -> "./images/icon_success_sml.gif")
    def e3 = print(spec2) must \\("img", "src" -> "./images/icon_failure_sml.gif")
  }

  case class seeIt() extends LinkedSpecifications {
    val spec1: Fragments = "ex1" ! failure ^ "a " ~/ ("successfull spec", successfulSubSpec) ^ end
    val spec2: Fragments = "spec2".title ^ "ex1" ! success ^ "a " ~/ ("failed spec", failedSubSpec) ^ end

    def e1 = htmlLines(spec1) must have size(6)
    def e2 = print(spec1) must \\("img", "src" -> "./images/icon_success_sml.gif")
    def e3 = print(spec2) must \\("img", "src" -> "./images/icon_failure_sml.gif")
  }

  trait MockHtmlPrinter extends FragmentExecution with DefaultStoring { outer =>
    val fs = mock[FileSystem]
    val fileWriter = new MockFileWriter {}
    val out = fileWriter.getWriter
    override lazy val repository = mock[StatisticsRepository]

    repository.previousResult(any[SpecName], any[Example]) returns Some(success)
    repository.getStatistics(any[SpecName]) returns Some(Stats())

    def printer = new HtmlPrinter { }

    def htmlLines(spec: Fragments) = {
      val executedSpec = store(args())(ExecutingSpecification.create(SpecName("spec"), spec.fragments.map(executeFragment)))
      printer.reduce(executedSpec.execute)
    }
    def print(spec: Fragments) = printer.sortByFile(SpecName("spec"), HtmlLink(SpecName("spec"), "", "spec"))(htmlLines(spec)).flatten.toSeq.head.printLines(new HtmlResultOutput).xml

    def printSpec(spec: SpecificationStructure) = {
      printer.print(ExecutingSpecification.create(spec.content.specName, spec.content.fragments.map(executeFragment)).execute)
      out.messages.mkString("\n")
    }

    /**
     * for the failed subspec, return the statistics as failed when it is linked with "seeOnly"
     */
    override protected def storeStats = (fn: (ExecutedFragment, SpecName)) => {
      fn match {
        case (ExecutedSpecStart(start @ SpecStart(n,_,_,true), loc, st), name) if n.title == "failedSubSpec" =>
          ExecutedSpecStart(start, loc, Stats(failures = 1))
        case (fragment, name) => fragment
      }
    }

  }
  def printer = new HtmlPrinter {}

}