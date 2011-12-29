package org.specs2
package specification

import ExecutedSpecificationData._
import org.scalacheck.{Gen, Arbitrary}
import text.TextData._
import java.util.concurrent.Executors
import execute.{ Error, Failure }
import internal.scalaz._
import Scalaz._
import concurrent._
import Strategy._
import control.NamedThreadFactory

trait ExecutingSpecificationData extends Data[ExecutingSpecification] {

  implicit lazy val arbExecutingSpecification: Arbitrary[ExecutingSpecification] = Arbitrary {

    def genExecutingSpecification = (size: Int) => {
      for {
        fragments     <- Gen.listOfN(size, genExecutingFragment)
        name          <- arbAsciiString.arbitrary
      }
      yield ExecutingSpecification(FinishedExecutingFragment(start(name)) +: fragments.toSeq :+ FinishedExecutingFragment(end(name)))
    }

    sizeOf1(genExecutingSpecification)
  }

  def genTimedExecutedFragment =
    for {
      executionTime <- Gen.choose(0, 100)
      f             <- arbExecutedFragment.arbitrary
    }
    yield () => { Thread.sleep(executionTime); f }

  def genExecutingFragment: Gen[ExecutingFragment] =
    Gen.frequency(
    (3, genTimedExecutedFragment.map(f => PromisedExecutingFragment(promise(f())))),
    (1, genTimedExecutedFragment.map(f => LazyExecutingFragment(f))),
    (4, genTimedExecutedFragment.map(f => FinishedExecutingFragment(f())))
    )


  implicit val executor = Executors.newFixedThreadPool(4, new NamedThreadFactory("specs2.ExecutionTest"))

  def shutdown() = executor.shutdown()
}

object ExecutingSpecificationData extends ExecutingSpecificationData