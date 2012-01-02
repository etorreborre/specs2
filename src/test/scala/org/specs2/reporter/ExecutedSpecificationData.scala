package org.specs2
package reporter

import specification._
import org.scalacheck._
import text.TextData._
import collection.Iterablex._
import SpecificationData._
import main.Arguments
import reporter._
import internal.scalaz._
import Scalaz._

trait ExecutedSpecificationData extends Data[ExecutedSpecification] with FragmentExecution with DefaultSequence with DefaultExecutionStrategy {
  def spec1 = ExecutedSpecification(SpecName(""), Seq())

  implicit val arguments = Arguments()
  implicit def arbExecutedFragment: Arbitrary[ExecutedFragment] = Arbitrary(arbFragment.arbitrary.map(f => execute(f)))

	def arbExecutedFragmentSeq = Arbitrary(arbExecutedFragment.arbitrary.map(f => Seq(f)))

	lazy val arbExecutedSpecificationFragments = Arbitrary(arbExecutedSpecification.arbitrary.map(_.fragments))

	def arbLinkedExecutedSpecificationFragments(seeOnly: Boolean = false): Arbitrary[Seq[ExecutedFragment]] = Arbitrary {
		for (spec <- arbExecutedSpecification.arbitrary) yield {
			spec.fragments match {
				case ExecutedSpecStart(SpecStart(n,a,l,so),loc,timer) +: rest => ExecutedSpecStart(SpecStart(n,a,Some(HtmlLink(n)),seeOnly), loc, timer) +: rest
				case other => other
			}
		}
	}

  implicit lazy val arbExecutedSpecification: Arbitrary[ExecutedSpecification] =
    Arbitrary { arbSpecification.arbitrary.map(spec => (spec |> sequence |> execute).execute)  }

  def start(name: String) = ExecutedSpecStart(SpecStart(SpecName(name)))
  def end(name: String)   = ExecutedSpecEnd(SpecEnd(SpecName(name)))
}

object ExecutedSpecificationData extends ExecutedSpecificationData
