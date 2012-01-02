package org.specs2
package reporter

import specification._
import ExecutedSpecificationData._
import org.scalacheck.{Gen, Arbitrary}
import text.TextData._
import java.util.concurrent.Executors
import internal.scalaz._
import Scalaz._
import concurrent._
import Strategy._
import control.NamedThreadFactory

trait ExecutingSpecificationData extends Data[ExecutingSpecification] {

  implicit val arbExecutingSpecification: Arbitrary[ExecutingSpecification] = arbExecutingSpecificationWithTime()

  def arbExecutingSpecificationWithTime(maxTime: Int = 100): Arbitrary[ExecutingSpecification] = Arbitrary {

    def genExecutingSpecification = (size: Int) => {
      for {
        fragments     <- Gen.listOfN(size, genExecutingFragment(maxTime))
        name          <- arbAsciiString.arbitrary
      }
      yield ExecutingSpecification(FinishedExecutingFragment(start(name)) +: fragments.toSeq :+ FinishedExecutingFragment(end(name)))
    }

    sizeOf1(genExecutingSpecification)
  }

  def genTimedExecutedFragment(maxTime: Int) =
    for {
      executionTime <- Gen.choose(0, maxTime)
      f             <- arbExecutedFragment.arbitrary
    }
    yield () => { Thread.sleep(executionTime); f }

  def genExecutingFragment(maxTime: Int): Gen[ExecutingFragment] =
    Gen.frequency(
    (3, genTimedExecutedFragment(maxTime).map(f => PromisedExecutingFragment(promise(f())))),
    (1, genTimedExecutedFragment(maxTime).map(f => LazyExecutingFragment(f))),
    (4, genTimedExecutedFragment(maxTime).map(f => FinishedExecutingFragment(f())))
    )


  implicit val executor = Executors.newFixedThreadPool(4, new NamedThreadFactory("specs2.ExecutionTest"))

  def shutdown() = executor.shutdown()
}

object ExecutingSpecificationData extends ExecutingSpecificationData