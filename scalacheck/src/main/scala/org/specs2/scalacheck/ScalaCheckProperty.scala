package org.specs2
package scalacheck

import org.scalacheck._
import org.scalacheck.util.Pretty
import org.scalacheck.util.Pretty._
import execute._
import matcher._
import ScalaCheckProperty._
import specification._

import scalaz.{Failure => _, Success => _}

/**
 * A ScalaCheckProperty encapsulate a function to test with ScalaCheck
 *
 * Various typeclass instances are required:
 *
 *  - Arbitrary to generate values
 *  - Shrink to shrink counter-examples
 *  - Show to display arguments in case of a counter-example
 *  - Collector to collect values and provide a summary as string (to show frequencies for example)
 *
 *  A Context can be added to setup/teardown state before/after/around each property execution
 */

trait ScalaCheckProperty {
  type SelfType

  def prop: Prop
  def noShrink: SelfType

  def context: Option[Context]

  def setContext(context: Context): SelfType

  def before(action: =>Any): SelfType =
    setContext(Before.create(action))

  def after(action: =>Any): SelfType =
    setContext(After.create(action))

  def beforeAfter(beforeAction: =>Any, afterAction: =>Any): SelfType =
    setContext(BeforeAfter.create(beforeAction, afterAction))

  def around(action: Result => Result): SelfType =
    setContext(Around.create(action))

  def parameters: Parameters

  def setParameters(ps: Parameters): SelfType

  def set(minTestsOk: Int             = parameters.minTestsOk,
          minSize: Int                = parameters.minSize,
          maxDiscardRatio: Float      = parameters.maxDiscardRatio,
          maxSize: Int                = parameters.maxSize,
          workers: Int                = parameters.workers,
          rng: scala.util.Random      = parameters.rng,
          callback: Test.TestCallback = parameters.testCallback,
          loader: Option[ClassLoader] = parameters.loader
           ):SelfType =
    setParameters(
      parameters.copy(
        minTestsOk = minTestsOk,
        minSize = minSize,
        maxDiscardRatio = maxDiscardRatio,
        maxSize = maxSize,
        workers = workers,
        rng = rng,
        testCallback = callback,
        loader = loader
      ))

  def verbose: SelfType =
    setVerbosity(1)

  def setVerbosity(v: Int): SelfType

}

case class ScalaCheckProp[T, R](execute: T => R,
                                arbitrary: Arbitrary[T], shrink: Option[Shrink[T]],
                                pretty: T => Pretty,
                                collector: Option[T => Any],
                                asResult: AsResult[R],
                                context: Option[Context],
                                parameters: Parameters) extends ScalaCheckProperty {

  type SelfType = ScalaCheckProp[T, R]

  private implicit val asResult1  = asResult
  private implicit val arbitrary1 = arbitrary
  private implicit val shrink1    = shrink
  private implicit val pretty1    = Pretty.defaultParams

  lazy val propFunction = (t: T) => {
    lazy val executed = execute(t)
    context.foreach(_(executed))
    val prop = executed match {
      case p: Prop => p
      case r       => AsResultProp.asResultToProp(r)
    }
    collectValue(t, collector)(prop)
  }

  lazy val prop: Prop =
    shrink.fold(Prop.forAllNoShrink(propFunction))(_ => Prop.forAll(propFunction))

  def noShrink: SelfType = copy(shrink = None)

  def setArbitrary(arbitrary: Arbitrary[T]): SelfType =
    copy(arbitrary = arbitrary)

  def setShrink(shrink: Shrink[T]): SelfType =
    copy(shrink = Some(shrink))

  def setPretty(pretty: T => Pretty): SelfType =
    copy(pretty = pretty)

  def pretty(pretty: T => String): SelfType =
    setPretty((t: T) => Pretty(_ => pretty(t)))

  def setParameters(ps: Parameters): SelfType =
    copy(parameters = ps)

  def collect(f: T => Any): SelfType =
    copy(collector = Some(f))

  def prepare(action: T => T): SelfType =
    copy(execute = (t: T) => execute(action(t)))

  def setContext(context: Context): SelfType =
    copy(context = Some(context))

  def setVerbosity(v: Int): SelfType =
    setParameters(parameters.setVerbosity(v))
}

case class ScalaCheckProp2[T1, T2, R](
  execute: (T1, T2) => R,
  arbitrary1: Arbitrary[T1], arbitrary2: Arbitrary[T2],
  shrink1: Option[Shrink[T1]], shrink2: Option[Shrink[T2]],
  pretty1: T1 => Pretty, pretty2: T2 => Pretty,
  collector1: Option[T1 => Any], collector2: Option[T2 => Any],
  asResult: AsResult[R],
  context: Option[Context],
  parameters: Parameters) extends ScalaCheckProperty {

  type SelfType = ScalaCheckProp2[T1, T2, R]

  private implicit val asResult1  = asResult
  private implicit val (arb1, arb2) = (arbitrary1, arbitrary2)
  private implicit val (sh1, sh2)   = (shrink1, shrink2)
  private implicit val (pr1, pr2)   = (pretty1, pretty2)


  lazy val propFunction = (t1: T1, t2: T2) => {
    lazy val executed = execute(t1, t2)
    context.foreach(_(executed))
    val prop = executed match {
      case p: Prop => p
      case r       => AsResultProp.asResultToProp(r)
    }
    collectValue(t1, collector1)(collectValue(t2, collector2)(prop))
  }

  lazy val prop: Prop =
    shrink1.fold(Prop.forAllNoShrink(propFunction))(_ => Prop.forAll(propFunction))

  def noShrink: SelfType = copy(shrink1 = None, shrink2 = None)

  def setArbitrary1(a1: Arbitrary[T1]): SelfType = copy(arbitrary1 = a1)
  def setArbitrary2(a2: Arbitrary[T2]): SelfType = copy(arbitrary2 = a2)
  def setArbitraries(a1: Arbitrary[T1], a2: Arbitrary[T2]): SelfType =
    setArbitrary1(a1).setArbitrary2(a2)


  def setShrink1(s1: Shrink[T1]): SelfType = copy(shrink1 = Some(s1))
  def setShrink2(s2: Shrink[T2]): SelfType = copy(shrink2 = Some(s2))

  def setShrinks(s1: Shrink[T1], s2: Shrink[T2]): SelfType =
    setShrink1(s1).setShrink2(s2)

  def setPretty1(p1: T1 => Pretty): SelfType = copy(pretty1 = p1)
  def pretty1(p1: T1 => String): SelfType = setPretty1((t1: T1) => Pretty(_ => p1(t1)))

  def setPretty2(p2: T2 => Pretty): SelfType = copy(pretty2 = p2)
  def pretty2(p2: T2 => String): SelfType = setPretty2((t2: T2) => Pretty(_ => p2(t2)))

  def setPretties(p1: T1 => Pretty, p2: T2 => Pretty): SelfType =
    setPretty1(p1).setPretty2(p2)

  def pretties(p1: T1 => String, p2: T2 => String): SelfType =
    pretty1(p1).pretty2(p2)

  def collect1(f: T1 => Any): SelfType = copy(collector1 = Some(f))
  def collect2(f: T2 => Any): SelfType = copy(collector2 = Some(f))
  def collectAll(f1: T1 => Any, f2: T2 => Any): SelfType =
    collect1(f1).collect2(f2)

  def prepare(action: (T1, T2) => (T1, T2)): SelfType =
    copy(execute = (t1: T1, t2: T2) => execute.tupled(action(t1, t2)))

  def setContext(context: Context): SelfType = copy(context = Some(context))

  def setParameters(ps: Parameters): SelfType = copy(parameters = ps)

  def setVerbosity(v: Int): SelfType =
    setParameters(parameters.setVerbosity(v))

}

object ScalaCheckProperty {
  /** @return a Prop that will collect the value if a collector is defined */
  def collectValue[T](t: T, collector: Option[T => Any]): Prop => Prop = (prop: Prop) =>
    collector.fold(prop)(c => Prop.collect(c(t))(prop))
}
