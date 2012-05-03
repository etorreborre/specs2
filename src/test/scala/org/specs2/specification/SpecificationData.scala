package org.specs2
package specification

import org.scalacheck._
import text.TextData._
import execute.StandardResults._
import collection.Iterablex._
import specification.StandardFragments._
import execute.Executable

trait SpecificationData extends Data[Specification] {

  implicit def arbFragment: Arbitrary[Fragment] = Arbitrary {
    Gen.frequency (
      (2, Gen.value(Text("text"))),
      (1, Gen.value(Example("success", success))),
      (1, Gen.value(Example("failure", failure))),
      (1, Gen.value(Example("error", anError))),
      (1, Gen.value(Example("skipped", skipped))),
      (1, Gen.value(Example("pending", pending))),
      (1, Gen.value(Step("step"))),
      (1, Gen.value(Action("action"))),
      (1, Gen.value(Br())),
      (1, Gen.value(Backtab())))
  }

  def arbFragmentSeq: Arbitrary[Seq[Fragment]] = Arbitrary {
    for (f <- arbFragment.arbitrary) yield Seq(f)
  }

  lazy val arbSpecificationFragments: Arbitrary[Seq[Fragment]] = Arbitrary {
    for (spec <- arbSpecification.arbitrary) yield spec.content.fragments
  }

  def arbLinkedSpecificationFragments(seeOnly: Boolean = false): Arbitrary[Seq[Fragment]] = Arbitrary {
    for (spec <- arbSpecification.arbitrary) yield {
      spec.content.fragments match {
        case SpecStart(n,a,Linked(l,so,h)) +: rest => SpecStart(n,a,Linked(Some(HtmlLink(n)),seeOnly,h)) +: rest
        case other                          => other
      }
    }
  }

  implicit lazy val arbSpecification: Arbitrary[Specification] = Arbitrary {

    def genSpecification = (size: Int) => {
      for {
        includedSpecNb      <- Gen.choose(0, 0)
        fragmentOrIncluded  <- Gen.listOfN(size, Gen.frequency(
          (includedSpecNb, arbLinkedSpecificationFragments(seeOnly = false).arbitrary),
          (includedSpecNb, arbLinkedSpecificationFragments(seeOnly = true).arbitrary),
          (10,             arbFragmentSeq.arbitrary)))
        specTitle           <- arbAsciiString.arbitrary
      }
      yield new Specification { def is = specTitle.title ^ fragmentOrIncluded.flatten }
    }
    sizeOf1(genSpecification)

  }

  def arbTimedSpecification(maxTime: Int = 100): Arbitrary[Specification] = Arbitrary {
    for {
      executionTime <- Gen.choose(0, maxTime)
      spec          <- arbSpecification.arbitrary
    } yield new Specification { def is = spec.content.map(addTime(executionTime)) }
  }

  def addTime(time: Int): Fragment => Fragment = (f: Fragment) => {
    f match {
      case e: Executable => e.map(b => {Thread.sleep(time); b}).asInstanceOf[Fragment]
      case other         => other
    }
  }
}
object SpecificationData extends SpecificationData
