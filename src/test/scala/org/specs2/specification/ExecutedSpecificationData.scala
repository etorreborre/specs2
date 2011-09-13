package org.specs2
package specification

import org.scalacheck._
import Gen._
import text.TextData._
import main.Arguments
import execute.StandardResults._
import FormattingFragments._
import collection.Iterablex._

trait ExecutedSpecificationData extends Data[ExecutedSpecification] {
  def spec1 = ExecutedSpecification(SpecName(""), Seq())

  implicit def arbExecutedFragment: Arbitrary[ExecutedFragment] = Arbitrary {
    Gen.frequency (
      (10, Gen.value(ExecutedText("text"))),
      (8, Gen.value(ExecutedResult("success", success))),
      (1, Gen.value(ExecutedResult("failure", failure))),
      (1, Gen.value(ExecutedResult("error", anError))),
      (1, Gen.value(ExecutedResult("skipped", skipped))),
      (1, Gen.value(ExecutedResult("pending", pending))),
      (1, Gen.value(ExecutedNoText())),
      (2, Gen.value(ExecutedBr())),
      (1, Gen.value(ExecutedBacktab())))
  }

	def arbExecutedFragmentSeq: Arbitrary[Seq[ExecutedFragment]] = Arbitrary {
		for (f <- arbExecutedFragment.arbitrary) yield Seq(f)
	}

	lazy val arbExecutedSpecificationFragments: Arbitrary[Seq[ExecutedFragment]] = Arbitrary {
		for (spec <- arbExecutedSpecification.arbitrary) yield spec.fragments
	}
	
	def arbLinkedExecutedSpecificationFragments(seeOnly: Boolean = false): Arbitrary[Seq[ExecutedFragment]] = Arbitrary {
		for (spec <- arbExecutedSpecification.arbitrary) yield {
			spec.fragments match {
				case ExecutedSpecStart(SpecStart(n,a,l,so),loc,timer) +: rest => ExecutedSpecStart(SpecStart(n,a,Some(HtmlLink(n)),seeOnly), loc, timer) +: rest
				case other => other
			}
		}	
	}

  implicit lazy val arbExecutedSpecification: Arbitrary[ExecutedSpecification] = Arbitrary {

    def genExecutedSpecification(size: Int): Gen[ExecutedSpecification] =
      for {
        includedSpecNb      <- Gen.choose(0, 0)
				fragmentOrIncluded  <- Gen.listOfN(size, Gen.frequency(
				                          (includedSpecNb, arbLinkedExecutedSpecificationFragments(seeOnly = false).arbitrary),
				                          (includedSpecNb, arbLinkedExecutedSpecificationFragments(seeOnly = true).arbitrary),
				                          (10, arbExecutedFragmentSeq.arbitrary)))
        name                <- arbAsciiString.arbitrary
      }
      yield new ExecutedSpecification(SpecName(name),
                                      ExecutedSpecStart(SpecStart(SpecName(name))) +: fragmentOrIncluded.flatten :+ ExecutedSpecEnd(SpecEnd(SpecName(name))))

    def sizedList(size: Int): Gen[ExecutedSpecification] = {
      if (size <= 0) genExecutedSpecification(1)
      else           genExecutedSpecification(size)
    }
    Gen.sized(size => sizedList(size))
  }


}
object ExecutedSpecificationData extends ExecutedSpecificationData
