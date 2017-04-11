package org.specs2
package scalacheck

import org.scalacheck._
import org.scalacheck.util.{FreqMap, Pretty}
import org.scalacheck.util.Pretty._
import execute._
import specification._
import AsResultProp._
import ScalaCheckProperty._
import org.specs2.specification.core.{AsExecution, Execution}

/**
 * A ScalaCheckProperty encapsulates a ScalaCheck Prop and its parameters
 */
trait ScalaCheckProperty {
  type SelfType <: ScalaCheckProperty

  def prop: Prop

  def parameters: Parameters

  def prettyFreqMap: FreqMap[Set[Any]] => Pretty

  def setParameters(ps: Parameters): SelfType

  def setVerbosity(v: Int): SelfType =
    setParameters(parameters.setVerbosity(v))

  def set(minTestsOk: Int             = parameters.minTestsOk,
          minSize: Int                = parameters.minSize,
          maxDiscardRatio: Float      = parameters.maxDiscardRatio,
          maxSize: Int                = parameters.maxSize,
          workers: Int                = parameters.workers,
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
        testCallback = callback,
        loader = loader
      ))

  def display(minTestsOk: Int             = parameters.minTestsOk,
               minSize: Int                = parameters.minSize,
               maxDiscardRatio: Float      = parameters.maxDiscardRatio,
               maxSize: Int                = parameters.maxSize,
               workers: Int                = parameters.workers,
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
        testCallback = callback,
        loader = loader
      ).setVerbosity(1))

  def verbose: SelfType =
    setVerbosity(1)

  def setPrettyFreqMap(f: FreqMap[Set[Any]] => Pretty): SelfType

  def prettyFreqMap(f: FreqMap[Set[Any]] => String): SelfType =
    setPrettyFreqMap((fq: FreqMap[Set[Any]]) => Pretty(_ => f(fq)))
}

/**
 * A ScalaCheckFunction adds the possibility to select various typeclass instances for a given property:
 *
 *  - Arbitrary to generate values
 *  - Shrink to shrink counter-examples
 *  - Show to display arguments in case of a counter-example
 *  - Collector to collect values and provide a summary as string (to show frequencies for example)
 *
 *  A Context can be added to setup/teardown state before/after/around each property execution
 */
trait ScalaCheckFunction extends ScalaCheckProperty {
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

  protected def executeInContext[R : AsResult](result: =>R) = {
    lazy val executed = result
    context.foreach(_(executed))
    executed match {
      case p: Prop => p
      case other   => AsResultProp.asResultToProp(other)
    }
  }

}

case class ScalaCheckFunction1[T, R](execute: T => R,
                                 arbitrary: Arbitrary[T], shrink: Option[Shrink[T]],
                                 collectors: List[T => Any],
                                 pretty: T => Pretty, prettyFreqMap: FreqMap[Set[Any]] => Pretty,
                                 asResult: AsResult[R],
                                 context: Option[Context],
                                 parameters: Parameters) extends ScalaCheckFunction {

  type SelfType = ScalaCheckFunction1[T, R]

  private implicit val asResult1  = asResult
  private implicit val arbitrary1 = arbitrary
  private implicit val pretty1    = pretty

  lazy val propFunction = (t: T) => {
    lazy val executed = execute(t)
    executeInContext(executed)
    collectors.foldLeft(asResultToProp(executed))((p, c) => Prop.collect(c(t))(p))
  }

  lazy val prop: Prop =
    shrink.fold(Prop.forAllNoShrink(propFunction))(_ => Prop.forAll(propFunction))

  def noShrink: SelfType = copy(shrink = None)

  def setArbitrary(arbitrary: Arbitrary[T]): SelfType =
    copy(arbitrary = arbitrary)

  def setGen(gen: Gen[T]): SelfType =
    setArbitrary(Arbitrary(gen))

  def setShrink(shrink: Shrink[T]): SelfType =
    copy(shrink = Some(shrink))

  def setPretty(pretty: T => Pretty): SelfType =
    copy(pretty = pretty)

  def pretty(pretty: T => String): SelfType =
    setPretty((t: T) => Pretty(_ => pretty(t)))

  def setPrettyFreqMap(f: FreqMap[Set[Any]] => Pretty): SelfType =
    copy(prettyFreqMap = f)

  def setParameters(ps: Parameters): SelfType =
    copy(parameters = ps)

  def collect: SelfType =
    collectArg(_.toString)

  def collectArg(f: T => Any): SelfType =
    copy(collectors = collectors :+ f)

  def prepare(action: T => T): SelfType =
    copy(execute = (t: T) => execute(action(t)))

  def setContext(context: Context): SelfType =
    copy(context = Some(context))

}


case class ScalaCheckFunction2[T1, T2, R](
                                           execute: (T1, T2) => R,
                                           argInstances1: ScalaCheckArgInstances[T1], argInstances2: ScalaCheckArgInstances[T2],
                                           prettyFreqMap: FreqMap[Set[Any]] => Pretty,
                                           asResult: AsResult[R],
                                           context: Option[Context],
                                           parameters: Parameters) extends ScalaCheckFunction {

  type SelfType = ScalaCheckFunction2[T1, T2, R]

  private implicit val asResult1  = asResult
  private implicit val (arb1,arb2) = (argInstances1.arbitrary,argInstances2.arbitrary)
  private implicit val (sh1,sh2) = (argInstances1.shrink,argInstances2.shrink)
  private implicit val (pr1,pr2) = (argInstances1.pretty,argInstances2.pretty)

  lazy val propFunction = (t1: T1, t2: T2) => {
    lazy val executed = execute(t1, t2)
    executeInContext(executed)
    argInstances1.collect(t1, argInstances2.collect(t2, asResultToProp(executed)))
  }

  lazy val prop: Prop =
    makeProp((t1: T1) => makeProp((t2: T2) => propFunction(t1, t2), argInstances2.shrink), argInstances1.shrink)

  def noShrink: SelfType = copy(argInstances1 = argInstances1.copy(shrink = None), argInstances2 = argInstances2.copy(shrink = None))

  def setArbitrary1(a1: Arbitrary[T1]): SelfType = copy(argInstances1 = argInstances1.copy(arbitrary = a1))
  def setArbitrary2(a2: Arbitrary[T2]): SelfType = copy(argInstances2 = argInstances2.copy(arbitrary = a2))
  def setArbitraries(a1: Arbitrary[T1], a2: Arbitrary[T2]): SelfType =
    setArbitrary1(a1).setArbitrary2(a2)

  def setGen1(g1: Gen[T1]): SelfType = setArbitrary1(Arbitrary(g1))
  def setGen2(g2: Gen[T2]): SelfType = setArbitrary2(Arbitrary(g2))
  def setGens(g1: Gen[T1], g2: Gen[T2]): SelfType =
    setGen1(g1).setGen2(g2)

  def setShrink1(s1: Shrink[T1]): SelfType = copy(argInstances1 = argInstances1.copy(shrink = Some(s1)))
  def setShrink2(s2: Shrink[T2]): SelfType = copy(argInstances2 = argInstances2.copy(shrink = Some(s2)))
  def setShrinks(s1: Shrink[T1], s2: Shrink[T2]): SelfType =
    setShrink1(s1).setShrink2(s2)

  def setPretty1(p1: T1 => Pretty): SelfType = copy(argInstances1 = argInstances1.copy(pretty = p1))
  def setPretty2(p2: T2 => Pretty): SelfType = copy(argInstances2 = argInstances2.copy(pretty = p2))
  def pretty1(p1: T1 => String): SelfType = setPretty1((t1: T1) => Pretty(_ => p1(t1)))
  def pretty2(p2: T2 => String): SelfType = setPretty2((t2: T2) => Pretty(_ => p2(t2)))

  def setPretties(p1: T1 => Pretty, p2: T2 => Pretty): SelfType =
    setPretty1(p1).setPretty2(p2)
  def pretties(p1: T1 => String, p2: T2 => String): SelfType =
    pretty1(p1).pretty2(p2)

  def setPrettyFreqMap(f: FreqMap[Set[Any]] => Pretty): SelfType =
    copy(prettyFreqMap = f)

  def collectArg1(f: T1 => Any): SelfType = copy(argInstances1 = argInstances1.copy(collectors = argInstances1.collectors :+ f))
  def collectArg2(f: T2 => Any): SelfType = copy(argInstances2 = argInstances2.copy(collectors = argInstances2.collectors :+ f))
  def collect1: SelfType = collectArg1(_.toString)
  def collect2: SelfType = collectArg2(_.toString)
  def collectAllArgs(f1: T1 => Any,f2: T2 => Any): SelfType =
    collectArg1(f1).collectArg2(f2)

  def collectAll: SelfType =
    collect1.collect2

  def prepare(action: (T1, T2) => (T1, T2)): SelfType =
    copy(execute = (t1: T1, t2: T2) => execute.tupled(action(t1, t2)))

  def setContext(context: Context): SelfType = copy(context = Some(context))

  def setParameters(ps: Parameters): SelfType = copy(parameters = ps)
}


case class ScalaCheckFunction3[T1, T2, T3, R](
                                               execute: (T1, T2, T3) => R,
                                               argInstances1: ScalaCheckArgInstances[T1], argInstances2: ScalaCheckArgInstances[T2], argInstances3: ScalaCheckArgInstances[T3],
                                               prettyFreqMap: FreqMap[Set[Any]] => Pretty,
                                               asResult: AsResult[R],
                                               context: Option[Context],
                                               parameters: Parameters) extends ScalaCheckFunction {

  type SelfType = ScalaCheckFunction3[T1, T2, T3, R]

  private implicit val asResult1  = asResult
  private implicit val (arb1,arb2,arb3) = (argInstances1.arbitrary,argInstances2.arbitrary,argInstances3.arbitrary)
  private implicit val (sh1,sh2,sh3) = (argInstances1.shrink,argInstances2.shrink,argInstances3.shrink)
  private implicit val (pr1,pr2,pr3) = (argInstances1.pretty,argInstances2.pretty,argInstances3.pretty)

  lazy val propFunction = (t1: T1, t2: T2, t3: T3) => {
    lazy val executed = execute(t1, t2, t3)
    executeInContext(executed)
    argInstances1.collect(t1, argInstances2.collect(t2, argInstances3.collect(t3, asResultToProp(executed))))
  }

  lazy val prop: Prop =
    makeProp((t1: T1) => makeProp((t2: T2) => makeProp((t3: T3) => propFunction(t1, t2, t3), argInstances3.shrink), argInstances2.shrink), argInstances1.shrink)

  def noShrink: SelfType = copy(argInstances1 = argInstances1.copy(shrink = None), argInstances2 = argInstances2.copy(shrink = None), argInstances3 = argInstances3.copy(shrink = None))

  def setArbitrary1(a1: Arbitrary[T1]): SelfType = copy(argInstances1 = argInstances1.copy(arbitrary = a1))
  def setArbitrary2(a2: Arbitrary[T2]): SelfType = copy(argInstances2 = argInstances2.copy(arbitrary = a2))
  def setArbitrary3(a3: Arbitrary[T3]): SelfType = copy(argInstances3 = argInstances3.copy(arbitrary = a3))
  def setArbitraries(a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3]): SelfType =
    setArbitrary1(a1).setArbitrary2(a2).setArbitrary3(a3)

  def setGen1(g1: Gen[T1]): SelfType = setArbitrary1(Arbitrary(g1))
  def setGen2(g2: Gen[T2]): SelfType = setArbitrary2(Arbitrary(g2))
  def setGen3(g3: Gen[T3]): SelfType = setArbitrary3(Arbitrary(g3))
  def setGens(g1: Gen[T1], g2: Gen[T2], g3: Gen[T3]): SelfType =
    setGen1(g1).setGen2(g2).setGen3(g3)

  def setShrink1(s1: Shrink[T1]): SelfType = copy(argInstances1 = argInstances1.copy(shrink = Some(s1)))
  def setShrink2(s2: Shrink[T2]): SelfType = copy(argInstances2 = argInstances2.copy(shrink = Some(s2)))
  def setShrink3(s3: Shrink[T3]): SelfType = copy(argInstances3 = argInstances3.copy(shrink = Some(s3)))
  def setShrinks(s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3]): SelfType =
    setShrink1(s1).setShrink2(s2).setShrink3(s3)

  def setPretty1(p1: T1 => Pretty): SelfType = copy(argInstances1 = argInstances1.copy(pretty = p1))
  def setPretty2(p2: T2 => Pretty): SelfType = copy(argInstances2 = argInstances2.copy(pretty = p2))
  def setPretty3(p3: T3 => Pretty): SelfType = copy(argInstances3 = argInstances3.copy(pretty = p3))
  def pretty1(p1: T1 => String): SelfType = setPretty1((t1: T1) => Pretty(_ => p1(t1)))
  def pretty2(p2: T2 => String): SelfType = setPretty2((t2: T2) => Pretty(_ => p2(t2)))
  def pretty3(p3: T3 => String): SelfType = setPretty3((t3: T3) => Pretty(_ => p3(t3)))

  def setPretties(p1: T1 => Pretty, p2: T2 => Pretty, p3: T3 => Pretty): SelfType =
    setPretty1(p1).setPretty2(p2).setPretty3(p3)
  def pretties(p1: T1 => String, p2: T2 => String, p3: T3 => String): SelfType =
    pretty1(p1).pretty2(p2).pretty3(p3)

  def setPrettyFreqMap(f: FreqMap[Set[Any]] => Pretty): SelfType =
    copy(prettyFreqMap = f)

  def collectArg1(f: T1 => Any): SelfType = copy(argInstances1 = argInstances1.copy(collectors = argInstances1.collectors :+ f))
  def collectArg2(f: T2 => Any): SelfType = copy(argInstances2 = argInstances2.copy(collectors = argInstances2.collectors :+ f))
  def collectArg3(f: T3 => Any): SelfType = copy(argInstances3 = argInstances3.copy(collectors = argInstances3.collectors :+ f))
  def collect1: SelfType = collectArg1(_.toString)
  def collect2: SelfType = collectArg2(_.toString)
  def collect3: SelfType = collectArg3(_.toString)
  def collectAllArgs(f1: T1 => Any,f2: T2 => Any,f3: T3 => Any): SelfType =
    collectArg1(f1).collectArg2(f2).collectArg3(f3)

  def collectAll: SelfType =
    collect1.collect2.collect3

  def prepare(action: (T1, T2, T3) => (T1, T2, T3)): SelfType =
    copy(execute = (t1: T1, t2: T2, t3: T3) => execute.tupled(action(t1, t2, t3)))

  def setContext(context: Context): SelfType = copy(context = Some(context))

  def setParameters(ps: Parameters): SelfType = copy(parameters = ps)
}


case class ScalaCheckFunction4[T1, T2, T3, T4, R](
                                                   execute: (T1, T2, T3, T4) => R,
                                                   argInstances1: ScalaCheckArgInstances[T1], argInstances2: ScalaCheckArgInstances[T2], argInstances3: ScalaCheckArgInstances[T3], argInstances4: ScalaCheckArgInstances[T4],
                                                   prettyFreqMap: FreqMap[Set[Any]] => Pretty,
                                                   asResult: AsResult[R],
                                                   context: Option[Context],
                                                   parameters: Parameters) extends ScalaCheckFunction {

  type SelfType = ScalaCheckFunction4[T1, T2, T3, T4, R]

  private implicit val asResult1  = asResult
  private implicit val (arb1,arb2,arb3,arb4) = (argInstances1.arbitrary,argInstances2.arbitrary,argInstances3.arbitrary,argInstances4.arbitrary)
  private implicit val (sh1,sh2,sh3,sh4) = (argInstances1.shrink,argInstances2.shrink,argInstances3.shrink,argInstances4.shrink)
  private implicit val (pr1,pr2,pr3,pr4) = (argInstances1.pretty,argInstances2.pretty,argInstances3.pretty,argInstances4.pretty)

  lazy val propFunction = (t1: T1, t2: T2, t3: T3, t4: T4) => {
    lazy val executed = execute(t1, t2, t3, t4)
    executeInContext(executed)
    argInstances1.collect(t1, argInstances2.collect(t2, argInstances3.collect(t3, argInstances4.collect(t4, asResultToProp(executed)))))
  }

  lazy val prop: Prop =
    makeProp((t1: T1) => makeProp((t2: T2) => makeProp((t3: T3) => makeProp((t4: T4) => propFunction(t1, t2, t3, t4), argInstances4.shrink), argInstances3.shrink), argInstances2.shrink), argInstances1.shrink)

  def noShrink: SelfType = copy(argInstances1 = argInstances1.copy(shrink = None), argInstances2 = argInstances2.copy(shrink = None), argInstances3 = argInstances3.copy(shrink = None), argInstances4 = argInstances4.copy(shrink = None))

  def setArbitrary1(a1: Arbitrary[T1]): SelfType = copy(argInstances1 = argInstances1.copy(arbitrary = a1))
  def setArbitrary2(a2: Arbitrary[T2]): SelfType = copy(argInstances2 = argInstances2.copy(arbitrary = a2))
  def setArbitrary3(a3: Arbitrary[T3]): SelfType = copy(argInstances3 = argInstances3.copy(arbitrary = a3))
  def setArbitrary4(a4: Arbitrary[T4]): SelfType = copy(argInstances4 = argInstances4.copy(arbitrary = a4))
  def setArbitraries(a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4]): SelfType =
    setArbitrary1(a1).setArbitrary2(a2).setArbitrary3(a3).setArbitrary4(a4)

  def setGen1(g1: Gen[T1]): SelfType = setArbitrary1(Arbitrary(g1))
  def setGen2(g2: Gen[T2]): SelfType = setArbitrary2(Arbitrary(g2))
  def setGen3(g3: Gen[T3]): SelfType = setArbitrary3(Arbitrary(g3))
  def setGen4(g4: Gen[T4]): SelfType = setArbitrary4(Arbitrary(g4))
  def setGens(g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4]): SelfType =
    setGen1(g1).setGen2(g2).setGen3(g3).setGen4(g4)

  def setShrink1(s1: Shrink[T1]): SelfType = copy(argInstances1 = argInstances1.copy(shrink = Some(s1)))
  def setShrink2(s2: Shrink[T2]): SelfType = copy(argInstances2 = argInstances2.copy(shrink = Some(s2)))
  def setShrink3(s3: Shrink[T3]): SelfType = copy(argInstances3 = argInstances3.copy(shrink = Some(s3)))
  def setShrink4(s4: Shrink[T4]): SelfType = copy(argInstances4 = argInstances4.copy(shrink = Some(s4)))
  def setShrinks(s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4]): SelfType =
    setShrink1(s1).setShrink2(s2).setShrink3(s3).setShrink4(s4)

  def setPretty1(p1: T1 => Pretty): SelfType = copy(argInstances1 = argInstances1.copy(pretty = p1))
  def setPretty2(p2: T2 => Pretty): SelfType = copy(argInstances2 = argInstances2.copy(pretty = p2))
  def setPretty3(p3: T3 => Pretty): SelfType = copy(argInstances3 = argInstances3.copy(pretty = p3))
  def setPretty4(p4: T4 => Pretty): SelfType = copy(argInstances4 = argInstances4.copy(pretty = p4))
  def pretty1(p1: T1 => String): SelfType = setPretty1((t1: T1) => Pretty(_ => p1(t1)))
  def pretty2(p2: T2 => String): SelfType = setPretty2((t2: T2) => Pretty(_ => p2(t2)))
  def pretty3(p3: T3 => String): SelfType = setPretty3((t3: T3) => Pretty(_ => p3(t3)))
  def pretty4(p4: T4 => String): SelfType = setPretty4((t4: T4) => Pretty(_ => p4(t4)))

  def setPretties(p1: T1 => Pretty, p2: T2 => Pretty, p3: T3 => Pretty, p4: T4 => Pretty): SelfType =
    setPretty1(p1).setPretty2(p2).setPretty3(p3).setPretty4(p4)
  def pretties(p1: T1 => String, p2: T2 => String, p3: T3 => String, p4: T4 => String): SelfType =
    pretty1(p1).pretty2(p2).pretty3(p3).pretty4(p4)

  def setPrettyFreqMap(f: FreqMap[Set[Any]] => Pretty): SelfType =
    copy(prettyFreqMap = f)

  def collectArg1(f: T1 => Any): SelfType = copy(argInstances1 = argInstances1.copy(collectors = argInstances1.collectors :+ f))
  def collectArg2(f: T2 => Any): SelfType = copy(argInstances2 = argInstances2.copy(collectors = argInstances2.collectors :+ f))
  def collectArg3(f: T3 => Any): SelfType = copy(argInstances3 = argInstances3.copy(collectors = argInstances3.collectors :+ f))
  def collectArg4(f: T4 => Any): SelfType = copy(argInstances4 = argInstances4.copy(collectors = argInstances4.collectors :+ f))
  def collect1: SelfType = collectArg1(_.toString)
  def collect2: SelfType = collectArg2(_.toString)
  def collect3: SelfType = collectArg3(_.toString)
  def collect4: SelfType = collectArg4(_.toString)
  def collectAllArgs(f1: T1 => Any,f2: T2 => Any,f3: T3 => Any,f4: T4 => Any): SelfType =
    collectArg1(f1).collectArg2(f2).collectArg3(f3).collectArg4(f4)

  def collectAll: SelfType =
    collect1.collect2.collect3.collect4

  def prepare(action: (T1, T2, T3, T4) => (T1, T2, T3, T4)): SelfType =
    copy(execute = (t1: T1, t2: T2, t3: T3, t4: T4) => execute.tupled(action(t1, t2, t3, t4)))

  def setContext(context: Context): SelfType = copy(context = Some(context))

  def setParameters(ps: Parameters): SelfType = copy(parameters = ps)
}


case class ScalaCheckFunction5[T1, T2, T3, T4, T5, R](
                                                       execute: (T1, T2, T3, T4, T5) => R,
                                                       argInstances1: ScalaCheckArgInstances[T1], argInstances2: ScalaCheckArgInstances[T2], argInstances3: ScalaCheckArgInstances[T3], argInstances4: ScalaCheckArgInstances[T4], argInstances5: ScalaCheckArgInstances[T5],
                                                       prettyFreqMap: FreqMap[Set[Any]] => Pretty,
                                                       asResult: AsResult[R],
                                                       context: Option[Context],
                                                       parameters: Parameters) extends ScalaCheckFunction {

  type SelfType = ScalaCheckFunction5[T1, T2, T3, T4, T5, R]

  private implicit val asResult1  = asResult
  private implicit val (arb1,arb2,arb3,arb4,arb5) = (argInstances1.arbitrary,argInstances2.arbitrary,argInstances3.arbitrary,argInstances4.arbitrary,argInstances5.arbitrary)
  private implicit val (sh1,sh2,sh3,sh4,sh5) = (argInstances1.shrink,argInstances2.shrink,argInstances3.shrink,argInstances4.shrink,argInstances5.shrink)
  private implicit val (pr1,pr2,pr3,pr4,pr5) = (argInstances1.pretty,argInstances2.pretty,argInstances3.pretty,argInstances4.pretty,argInstances5.pretty)

  lazy val propFunction = (t1: T1, t2: T2, t3: T3, t4: T4, t5: T5) => {
    lazy val executed = execute(t1, t2, t3, t4, t5)
    executeInContext(executed)
    argInstances1.collect(t1, argInstances2.collect(t2, argInstances3.collect(t3, argInstances4.collect(t4, argInstances5.collect(t5, asResultToProp(executed))))))
  }

  lazy val prop: Prop =
    makeProp((t1: T1) => makeProp((t2: T2) => makeProp((t3: T3) => makeProp((t4: T4) => makeProp((t5: T5) => propFunction(t1, t2, t3, t4, t5), argInstances5.shrink), argInstances4.shrink), argInstances3.shrink), argInstances2.shrink), argInstances1.shrink)

  def noShrink: SelfType = copy(argInstances1 = argInstances1.copy(shrink = None), argInstances2 = argInstances2.copy(shrink = None), argInstances3 = argInstances3.copy(shrink = None), argInstances4 = argInstances4.copy(shrink = None), argInstances5 = argInstances5.copy(shrink = None))

  def setArbitrary1(a1: Arbitrary[T1]): SelfType = copy(argInstances1 = argInstances1.copy(arbitrary = a1))
  def setArbitrary2(a2: Arbitrary[T2]): SelfType = copy(argInstances2 = argInstances2.copy(arbitrary = a2))
  def setArbitrary3(a3: Arbitrary[T3]): SelfType = copy(argInstances3 = argInstances3.copy(arbitrary = a3))
  def setArbitrary4(a4: Arbitrary[T4]): SelfType = copy(argInstances4 = argInstances4.copy(arbitrary = a4))
  def setArbitrary5(a5: Arbitrary[T5]): SelfType = copy(argInstances5 = argInstances5.copy(arbitrary = a5))
  def setArbitraries(a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5]): SelfType =
    setArbitrary1(a1).setArbitrary2(a2).setArbitrary3(a3).setArbitrary4(a4).setArbitrary5(a5)

  def setGen1(g1: Gen[T1]): SelfType = setArbitrary1(Arbitrary(g1))
  def setGen2(g2: Gen[T2]): SelfType = setArbitrary2(Arbitrary(g2))
  def setGen3(g3: Gen[T3]): SelfType = setArbitrary3(Arbitrary(g3))
  def setGen4(g4: Gen[T4]): SelfType = setArbitrary4(Arbitrary(g4))
  def setGen5(g5: Gen[T5]): SelfType = setArbitrary5(Arbitrary(g5))
  def setGens(g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5]): SelfType =
    setGen1(g1).setGen2(g2).setGen3(g3).setGen4(g4).setGen5(g5)

  def setShrink1(s1: Shrink[T1]): SelfType = copy(argInstances1 = argInstances1.copy(shrink = Some(s1)))
  def setShrink2(s2: Shrink[T2]): SelfType = copy(argInstances2 = argInstances2.copy(shrink = Some(s2)))
  def setShrink3(s3: Shrink[T3]): SelfType = copy(argInstances3 = argInstances3.copy(shrink = Some(s3)))
  def setShrink4(s4: Shrink[T4]): SelfType = copy(argInstances4 = argInstances4.copy(shrink = Some(s4)))
  def setShrink5(s5: Shrink[T5]): SelfType = copy(argInstances5 = argInstances5.copy(shrink = Some(s5)))
  def setShrinks(s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4], s5: Shrink[T5]): SelfType =
    setShrink1(s1).setShrink2(s2).setShrink3(s3).setShrink4(s4).setShrink5(s5)

  def setPretty1(p1: T1 => Pretty): SelfType = copy(argInstances1 = argInstances1.copy(pretty = p1))
  def setPretty2(p2: T2 => Pretty): SelfType = copy(argInstances2 = argInstances2.copy(pretty = p2))
  def setPretty3(p3: T3 => Pretty): SelfType = copy(argInstances3 = argInstances3.copy(pretty = p3))
  def setPretty4(p4: T4 => Pretty): SelfType = copy(argInstances4 = argInstances4.copy(pretty = p4))
  def setPretty5(p5: T5 => Pretty): SelfType = copy(argInstances5 = argInstances5.copy(pretty = p5))
  def pretty1(p1: T1 => String): SelfType = setPretty1((t1: T1) => Pretty(_ => p1(t1)))
  def pretty2(p2: T2 => String): SelfType = setPretty2((t2: T2) => Pretty(_ => p2(t2)))
  def pretty3(p3: T3 => String): SelfType = setPretty3((t3: T3) => Pretty(_ => p3(t3)))
  def pretty4(p4: T4 => String): SelfType = setPretty4((t4: T4) => Pretty(_ => p4(t4)))
  def pretty5(p5: T5 => String): SelfType = setPretty5((t5: T5) => Pretty(_ => p5(t5)))

  def setPretties(p1: T1 => Pretty, p2: T2 => Pretty, p3: T3 => Pretty, p4: T4 => Pretty, p5: T5 => Pretty): SelfType =
    setPretty1(p1).setPretty2(p2).setPretty3(p3).setPretty4(p4).setPretty5(p5)
  def pretties(p1: T1 => String, p2: T2 => String, p3: T3 => String, p4: T4 => String, p5: T5 => String): SelfType =
    pretty1(p1).pretty2(p2).pretty3(p3).pretty4(p4).pretty5(p5)

  def setPrettyFreqMap(f: FreqMap[Set[Any]] => Pretty): SelfType =
    copy(prettyFreqMap = f)

  def collectArg1(f: T1 => Any): SelfType = copy(argInstances1 = argInstances1.copy(collectors = argInstances1.collectors :+ f))
  def collectArg2(f: T2 => Any): SelfType = copy(argInstances2 = argInstances2.copy(collectors = argInstances2.collectors :+ f))
  def collectArg3(f: T3 => Any): SelfType = copy(argInstances3 = argInstances3.copy(collectors = argInstances3.collectors :+ f))
  def collectArg4(f: T4 => Any): SelfType = copy(argInstances4 = argInstances4.copy(collectors = argInstances4.collectors :+ f))
  def collectArg5(f: T5 => Any): SelfType = copy(argInstances5 = argInstances5.copy(collectors = argInstances5.collectors :+ f))
  def collect1: SelfType = collectArg1(_.toString)
  def collect2: SelfType = collectArg2(_.toString)
  def collect3: SelfType = collectArg3(_.toString)
  def collect4: SelfType = collectArg4(_.toString)
  def collect5: SelfType = collectArg5(_.toString)
  def collectAllArgs(f1: T1 => Any,f2: T2 => Any,f3: T3 => Any,f4: T4 => Any,f5: T5 => Any): SelfType =
    collectArg1(f1).collectArg2(f2).collectArg3(f3).collectArg4(f4).collectArg5(f5)

  def collectAll: SelfType =
    collect1.collect2.collect3.collect4.collect5

  def prepare(action: (T1, T2, T3, T4, T5) => (T1, T2, T3, T4, T5)): SelfType =
    copy(execute = (t1: T1, t2: T2, t3: T3, t4: T4, t5: T5) => execute.tupled(action(t1, t2, t3, t4, t5)))

  def setContext(context: Context): SelfType = copy(context = Some(context))

  def setParameters(ps: Parameters): SelfType = copy(parameters = ps)
}


case class ScalaCheckFunction6[T1, T2, T3, T4, T5, T6, R](
                                                           execute: (T1, T2, T3, T4, T5, T6) => R,
                                                           argInstances1: ScalaCheckArgInstances[T1], argInstances2: ScalaCheckArgInstances[T2], argInstances3: ScalaCheckArgInstances[T3], argInstances4: ScalaCheckArgInstances[T4], argInstances5: ScalaCheckArgInstances[T5], argInstances6: ScalaCheckArgInstances[T6],
                                                           prettyFreqMap: FreqMap[Set[Any]] => Pretty,
                                                           asResult: AsResult[R],
                                                           context: Option[Context],
                                                           parameters: Parameters) extends ScalaCheckFunction {

  type SelfType = ScalaCheckFunction6[T1, T2, T3, T4, T5, T6, R]

  private implicit val asResult1  = asResult
  private implicit val (arb1,arb2,arb3,arb4,arb5,arb6) = (argInstances1.arbitrary,argInstances2.arbitrary,argInstances3.arbitrary,argInstances4.arbitrary,argInstances5.arbitrary,argInstances6.arbitrary)
  private implicit val (sh1,sh2,sh3,sh4,sh5,sh6) = (argInstances1.shrink,argInstances2.shrink,argInstances3.shrink,argInstances4.shrink,argInstances5.shrink,argInstances6.shrink)
  private implicit val (pr1,pr2,pr3,pr4,pr5,pr6) = (argInstances1.pretty,argInstances2.pretty,argInstances3.pretty,argInstances4.pretty,argInstances5.pretty,argInstances6.pretty)

  lazy val propFunction = (t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6) => {
    lazy val executed = execute(t1, t2, t3, t4, t5, t6)
    executeInContext(executed)
    argInstances1.collect(t1, argInstances2.collect(t2, argInstances3.collect(t3, argInstances4.collect(t4, argInstances5.collect(t5, argInstances6.collect(t6, asResultToProp(executed)))))))
  }

  lazy val prop: Prop =
    makeProp((t1: T1) => makeProp((t2: T2) => makeProp((t3: T3) => makeProp((t4: T4) => makeProp((t5: T5) => makeProp((t6: T6) => propFunction(t1, t2, t3, t4, t5, t6), argInstances6.shrink), argInstances5.shrink), argInstances4.shrink), argInstances3.shrink), argInstances2.shrink), argInstances1.shrink)

  def noShrink: SelfType = copy(argInstances1 = argInstances1.copy(shrink = None), argInstances2 = argInstances2.copy(shrink = None), argInstances3 = argInstances3.copy(shrink = None), argInstances4 = argInstances4.copy(shrink = None), argInstances5 = argInstances5.copy(shrink = None), argInstances6 = argInstances6.copy(shrink = None))

  def setArbitrary1(a1: Arbitrary[T1]): SelfType = copy(argInstances1 = argInstances1.copy(arbitrary = a1))
  def setArbitrary2(a2: Arbitrary[T2]): SelfType = copy(argInstances2 = argInstances2.copy(arbitrary = a2))
  def setArbitrary3(a3: Arbitrary[T3]): SelfType = copy(argInstances3 = argInstances3.copy(arbitrary = a3))
  def setArbitrary4(a4: Arbitrary[T4]): SelfType = copy(argInstances4 = argInstances4.copy(arbitrary = a4))
  def setArbitrary5(a5: Arbitrary[T5]): SelfType = copy(argInstances5 = argInstances5.copy(arbitrary = a5))
  def setArbitrary6(a6: Arbitrary[T6]): SelfType = copy(argInstances6 = argInstances6.copy(arbitrary = a6))
  def setArbitraries(a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6]): SelfType =
    setArbitrary1(a1).setArbitrary2(a2).setArbitrary3(a3).setArbitrary4(a4).setArbitrary5(a5).setArbitrary6(a6)

  def setGen1(g1: Gen[T1]): SelfType = setArbitrary1(Arbitrary(g1))
  def setGen2(g2: Gen[T2]): SelfType = setArbitrary2(Arbitrary(g2))
  def setGen3(g3: Gen[T3]): SelfType = setArbitrary3(Arbitrary(g3))
  def setGen4(g4: Gen[T4]): SelfType = setArbitrary4(Arbitrary(g4))
  def setGen5(g5: Gen[T5]): SelfType = setArbitrary5(Arbitrary(g5))
  def setGen6(g6: Gen[T6]): SelfType = setArbitrary6(Arbitrary(g6))
  def setGens(g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6]): SelfType =
    setGen1(g1).setGen2(g2).setGen3(g3).setGen4(g4).setGen5(g5).setGen6(g6)

  def setShrink1(s1: Shrink[T1]): SelfType = copy(argInstances1 = argInstances1.copy(shrink = Some(s1)))
  def setShrink2(s2: Shrink[T2]): SelfType = copy(argInstances2 = argInstances2.copy(shrink = Some(s2)))
  def setShrink3(s3: Shrink[T3]): SelfType = copy(argInstances3 = argInstances3.copy(shrink = Some(s3)))
  def setShrink4(s4: Shrink[T4]): SelfType = copy(argInstances4 = argInstances4.copy(shrink = Some(s4)))
  def setShrink5(s5: Shrink[T5]): SelfType = copy(argInstances5 = argInstances5.copy(shrink = Some(s5)))
  def setShrink6(s6: Shrink[T6]): SelfType = copy(argInstances6 = argInstances6.copy(shrink = Some(s6)))
  def setShrinks(s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4], s5: Shrink[T5], s6: Shrink[T6]): SelfType =
    setShrink1(s1).setShrink2(s2).setShrink3(s3).setShrink4(s4).setShrink5(s5).setShrink6(s6)

  def setPretty1(p1: T1 => Pretty): SelfType = copy(argInstances1 = argInstances1.copy(pretty = p1))
  def setPretty2(p2: T2 => Pretty): SelfType = copy(argInstances2 = argInstances2.copy(pretty = p2))
  def setPretty3(p3: T3 => Pretty): SelfType = copy(argInstances3 = argInstances3.copy(pretty = p3))
  def setPretty4(p4: T4 => Pretty): SelfType = copy(argInstances4 = argInstances4.copy(pretty = p4))
  def setPretty5(p5: T5 => Pretty): SelfType = copy(argInstances5 = argInstances5.copy(pretty = p5))
  def setPretty6(p6: T6 => Pretty): SelfType = copy(argInstances6 = argInstances6.copy(pretty = p6))
  def pretty1(p1: T1 => String): SelfType = setPretty1((t1: T1) => Pretty(_ => p1(t1)))
  def pretty2(p2: T2 => String): SelfType = setPretty2((t2: T2) => Pretty(_ => p2(t2)))
  def pretty3(p3: T3 => String): SelfType = setPretty3((t3: T3) => Pretty(_ => p3(t3)))
  def pretty4(p4: T4 => String): SelfType = setPretty4((t4: T4) => Pretty(_ => p4(t4)))
  def pretty5(p5: T5 => String): SelfType = setPretty5((t5: T5) => Pretty(_ => p5(t5)))
  def pretty6(p6: T6 => String): SelfType = setPretty6((t6: T6) => Pretty(_ => p6(t6)))

  def setPretties(p1: T1 => Pretty, p2: T2 => Pretty, p3: T3 => Pretty, p4: T4 => Pretty, p5: T5 => Pretty, p6: T6 => Pretty): SelfType =
    setPretty1(p1).setPretty2(p2).setPretty3(p3).setPretty4(p4).setPretty5(p5).setPretty6(p6)
  def pretties(p1: T1 => String, p2: T2 => String, p3: T3 => String, p4: T4 => String, p5: T5 => String, p6: T6 => String): SelfType =
    pretty1(p1).pretty2(p2).pretty3(p3).pretty4(p4).pretty5(p5).pretty6(p6)

  def setPrettyFreqMap(f: FreqMap[Set[Any]] => Pretty): SelfType =
    copy(prettyFreqMap = f)

  def collectArg1(f: T1 => Any): SelfType = copy(argInstances1 = argInstances1.copy(collectors = argInstances1.collectors :+ f))
  def collectArg2(f: T2 => Any): SelfType = copy(argInstances2 = argInstances2.copy(collectors = argInstances2.collectors :+ f))
  def collectArg3(f: T3 => Any): SelfType = copy(argInstances3 = argInstances3.copy(collectors = argInstances3.collectors :+ f))
  def collectArg4(f: T4 => Any): SelfType = copy(argInstances4 = argInstances4.copy(collectors = argInstances4.collectors :+ f))
  def collectArg5(f: T5 => Any): SelfType = copy(argInstances5 = argInstances5.copy(collectors = argInstances5.collectors :+ f))
  def collectArg6(f: T6 => Any): SelfType = copy(argInstances6 = argInstances6.copy(collectors = argInstances6.collectors :+ f))
  def collect1: SelfType = collectArg1(_.toString)
  def collect2: SelfType = collectArg2(_.toString)
  def collect3: SelfType = collectArg3(_.toString)
  def collect4: SelfType = collectArg4(_.toString)
  def collect5: SelfType = collectArg5(_.toString)
  def collect6: SelfType = collectArg6(_.toString)
  def collectAllArgs(f1: T1 => Any,f2: T2 => Any,f3: T3 => Any,f4: T4 => Any,f5: T5 => Any,f6: T6 => Any): SelfType =
    collectArg1(f1).collectArg2(f2).collectArg3(f3).collectArg4(f4).collectArg5(f5).collectArg6(f6)

  def collectAll: SelfType =
    collect1.collect2.collect3.collect4.collect5.collect6

  def prepare(action: (T1, T2, T3, T4, T5, T6) => (T1, T2, T3, T4, T5, T6)): SelfType =
    copy(execute = (t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6) => execute.tupled(action(t1, t2, t3, t4, t5, t6)))

  def setContext(context: Context): SelfType = copy(context = Some(context))

  def setParameters(ps: Parameters): SelfType = copy(parameters = ps)
}


case class ScalaCheckFunction7[T1, T2, T3, T4, T5, T6, T7, R](
                                                               execute: (T1, T2, T3, T4, T5, T6, T7) => R,
                                                               argInstances1: ScalaCheckArgInstances[T1], argInstances2: ScalaCheckArgInstances[T2], argInstances3: ScalaCheckArgInstances[T3], argInstances4: ScalaCheckArgInstances[T4], argInstances5: ScalaCheckArgInstances[T5], argInstances6: ScalaCheckArgInstances[T6], argInstances7: ScalaCheckArgInstances[T7],
                                                               prettyFreqMap: FreqMap[Set[Any]] => Pretty,
                                                               asResult: AsResult[R],
                                                               context: Option[Context],
                                                               parameters: Parameters) extends ScalaCheckFunction {

  type SelfType = ScalaCheckFunction7[T1, T2, T3, T4, T5, T6, T7, R]

  private implicit val asResult1  = asResult
  private implicit val (arb1,arb2,arb3,arb4,arb5,arb6,arb7) = (argInstances1.arbitrary,argInstances2.arbitrary,argInstances3.arbitrary,argInstances4.arbitrary,argInstances5.arbitrary,argInstances6.arbitrary,argInstances7.arbitrary)
  private implicit val (sh1,sh2,sh3,sh4,sh5,sh6,sh7) = (argInstances1.shrink,argInstances2.shrink,argInstances3.shrink,argInstances4.shrink,argInstances5.shrink,argInstances6.shrink,argInstances7.shrink)
  private implicit val (pr1,pr2,pr3,pr4,pr5,pr6,pr7) = (argInstances1.pretty,argInstances2.pretty,argInstances3.pretty,argInstances4.pretty,argInstances5.pretty,argInstances6.pretty,argInstances7.pretty)

  lazy val propFunction = (t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7) => {
    lazy val executed = execute(t1, t2, t3, t4, t5, t6, t7)
    executeInContext(executed)
    argInstances1.collect(t1, argInstances2.collect(t2, argInstances3.collect(t3, argInstances4.collect(t4, argInstances5.collect(t5, argInstances6.collect(t6, argInstances7.collect(t7, asResultToProp(executed))))))))
  }

  lazy val prop: Prop =
    makeProp((t1: T1) => makeProp((t2: T2) => makeProp((t3: T3) => makeProp((t4: T4) => makeProp((t5: T5) => makeProp((t6: T6) => makeProp((t7: T7) => propFunction(t1, t2, t3, t4, t5, t6, t7), argInstances7.shrink), argInstances6.shrink), argInstances5.shrink), argInstances4.shrink), argInstances3.shrink), argInstances2.shrink), argInstances1.shrink)

  def noShrink: SelfType = copy(argInstances1 = argInstances1.copy(shrink = None), argInstances2 = argInstances2.copy(shrink = None), argInstances3 = argInstances3.copy(shrink = None), argInstances4 = argInstances4.copy(shrink = None), argInstances5 = argInstances5.copy(shrink = None), argInstances6 = argInstances6.copy(shrink = None), argInstances7 = argInstances7.copy(shrink = None))

  def setArbitrary1(a1: Arbitrary[T1]): SelfType = copy(argInstances1 = argInstances1.copy(arbitrary = a1))
  def setArbitrary2(a2: Arbitrary[T2]): SelfType = copy(argInstances2 = argInstances2.copy(arbitrary = a2))
  def setArbitrary3(a3: Arbitrary[T3]): SelfType = copy(argInstances3 = argInstances3.copy(arbitrary = a3))
  def setArbitrary4(a4: Arbitrary[T4]): SelfType = copy(argInstances4 = argInstances4.copy(arbitrary = a4))
  def setArbitrary5(a5: Arbitrary[T5]): SelfType = copy(argInstances5 = argInstances5.copy(arbitrary = a5))
  def setArbitrary6(a6: Arbitrary[T6]): SelfType = copy(argInstances6 = argInstances6.copy(arbitrary = a6))
  def setArbitrary7(a7: Arbitrary[T7]): SelfType = copy(argInstances7 = argInstances7.copy(arbitrary = a7))
  def setArbitraries(a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7]): SelfType =
    setArbitrary1(a1).setArbitrary2(a2).setArbitrary3(a3).setArbitrary4(a4).setArbitrary5(a5).setArbitrary6(a6).setArbitrary7(a7)

  def setGen1(g1: Gen[T1]): SelfType = setArbitrary1(Arbitrary(g1))
  def setGen2(g2: Gen[T2]): SelfType = setArbitrary2(Arbitrary(g2))
  def setGen3(g3: Gen[T3]): SelfType = setArbitrary3(Arbitrary(g3))
  def setGen4(g4: Gen[T4]): SelfType = setArbitrary4(Arbitrary(g4))
  def setGen5(g5: Gen[T5]): SelfType = setArbitrary5(Arbitrary(g5))
  def setGen6(g6: Gen[T6]): SelfType = setArbitrary6(Arbitrary(g6))
  def setGen7(g7: Gen[T7]): SelfType = setArbitrary7(Arbitrary(g7))
  def setGens(g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6], g7: Gen[T7]): SelfType =
    setGen1(g1).setGen2(g2).setGen3(g3).setGen4(g4).setGen5(g5).setGen6(g6).setGen7(g7)

  def setShrink1(s1: Shrink[T1]): SelfType = copy(argInstances1 = argInstances1.copy(shrink = Some(s1)))
  def setShrink2(s2: Shrink[T2]): SelfType = copy(argInstances2 = argInstances2.copy(shrink = Some(s2)))
  def setShrink3(s3: Shrink[T3]): SelfType = copy(argInstances3 = argInstances3.copy(shrink = Some(s3)))
  def setShrink4(s4: Shrink[T4]): SelfType = copy(argInstances4 = argInstances4.copy(shrink = Some(s4)))
  def setShrink5(s5: Shrink[T5]): SelfType = copy(argInstances5 = argInstances5.copy(shrink = Some(s5)))
  def setShrink6(s6: Shrink[T6]): SelfType = copy(argInstances6 = argInstances6.copy(shrink = Some(s6)))
  def setShrink7(s7: Shrink[T7]): SelfType = copy(argInstances7 = argInstances7.copy(shrink = Some(s7)))
  def setShrinks(s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4], s5: Shrink[T5], s6: Shrink[T6], s7: Shrink[T7]): SelfType =
    setShrink1(s1).setShrink2(s2).setShrink3(s3).setShrink4(s4).setShrink5(s5).setShrink6(s6).setShrink7(s7)

  def setPretty1(p1: T1 => Pretty): SelfType = copy(argInstances1 = argInstances1.copy(pretty = p1))
  def setPretty2(p2: T2 => Pretty): SelfType = copy(argInstances2 = argInstances2.copy(pretty = p2))
  def setPretty3(p3: T3 => Pretty): SelfType = copy(argInstances3 = argInstances3.copy(pretty = p3))
  def setPretty4(p4: T4 => Pretty): SelfType = copy(argInstances4 = argInstances4.copy(pretty = p4))
  def setPretty5(p5: T5 => Pretty): SelfType = copy(argInstances5 = argInstances5.copy(pretty = p5))
  def setPretty6(p6: T6 => Pretty): SelfType = copy(argInstances6 = argInstances6.copy(pretty = p6))
  def setPretty7(p7: T7 => Pretty): SelfType = copy(argInstances7 = argInstances7.copy(pretty = p7))
  def pretty1(p1: T1 => String): SelfType = setPretty1((t1: T1) => Pretty(_ => p1(t1)))
  def pretty2(p2: T2 => String): SelfType = setPretty2((t2: T2) => Pretty(_ => p2(t2)))
  def pretty3(p3: T3 => String): SelfType = setPretty3((t3: T3) => Pretty(_ => p3(t3)))
  def pretty4(p4: T4 => String): SelfType = setPretty4((t4: T4) => Pretty(_ => p4(t4)))
  def pretty5(p5: T5 => String): SelfType = setPretty5((t5: T5) => Pretty(_ => p5(t5)))
  def pretty6(p6: T6 => String): SelfType = setPretty6((t6: T6) => Pretty(_ => p6(t6)))
  def pretty7(p7: T7 => String): SelfType = setPretty7((t7: T7) => Pretty(_ => p7(t7)))

  def setPretties(p1: T1 => Pretty, p2: T2 => Pretty, p3: T3 => Pretty, p4: T4 => Pretty, p5: T5 => Pretty, p6: T6 => Pretty, p7: T7 => Pretty): SelfType =
    setPretty1(p1).setPretty2(p2).setPretty3(p3).setPretty4(p4).setPretty5(p5).setPretty6(p6).setPretty7(p7)
  def pretties(p1: T1 => String, p2: T2 => String, p3: T3 => String, p4: T4 => String, p5: T5 => String, p6: T6 => String, p7: T7 => String): SelfType =
    pretty1(p1).pretty2(p2).pretty3(p3).pretty4(p4).pretty5(p5).pretty6(p6).pretty7(p7)

  def setPrettyFreqMap(f: FreqMap[Set[Any]] => Pretty): SelfType =
    copy(prettyFreqMap = f)

  def collectArg1(f: T1 => Any): SelfType = copy(argInstances1 = argInstances1.copy(collectors = argInstances1.collectors :+ f))
  def collectArg2(f: T2 => Any): SelfType = copy(argInstances2 = argInstances2.copy(collectors = argInstances2.collectors :+ f))
  def collectArg3(f: T3 => Any): SelfType = copy(argInstances3 = argInstances3.copy(collectors = argInstances3.collectors :+ f))
  def collectArg4(f: T4 => Any): SelfType = copy(argInstances4 = argInstances4.copy(collectors = argInstances4.collectors :+ f))
  def collectArg5(f: T5 => Any): SelfType = copy(argInstances5 = argInstances5.copy(collectors = argInstances5.collectors :+ f))
  def collectArg6(f: T6 => Any): SelfType = copy(argInstances6 = argInstances6.copy(collectors = argInstances6.collectors :+ f))
  def collectArg7(f: T7 => Any): SelfType = copy(argInstances7 = argInstances7.copy(collectors = argInstances7.collectors :+ f))
  def collect1: SelfType = collectArg1(_.toString)
  def collect2: SelfType = collectArg2(_.toString)
  def collect3: SelfType = collectArg3(_.toString)
  def collect4: SelfType = collectArg4(_.toString)
  def collect5: SelfType = collectArg5(_.toString)
  def collect6: SelfType = collectArg6(_.toString)
  def collect7: SelfType = collectArg7(_.toString)
  def collectAllArgs(f1: T1 => Any,f2: T2 => Any,f3: T3 => Any,f4: T4 => Any,f5: T5 => Any,f6: T6 => Any,f7: T7 => Any): SelfType =
    collectArg1(f1).collectArg2(f2).collectArg3(f3).collectArg4(f4).collectArg5(f5).collectArg6(f6).collectArg7(f7)

  def collectAll: SelfType =
    collect1.collect2.collect3.collect4.collect5.collect6.collect7

  def prepare(action: (T1, T2, T3, T4, T5, T6, T7) => (T1, T2, T3, T4, T5, T6, T7)): SelfType =
    copy(execute = (t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7) => execute.tupled(action(t1, t2, t3, t4, t5, t6, t7)))

  def setContext(context: Context): SelfType = copy(context = Some(context))

  def setParameters(ps: Parameters): SelfType = copy(parameters = ps)
}


case class ScalaCheckFunction8[T1, T2, T3, T4, T5, T6, T7, T8, R](
                                                                   execute: (T1, T2, T3, T4, T5, T6, T7, T8) => R,
                                                                   argInstances1: ScalaCheckArgInstances[T1], argInstances2: ScalaCheckArgInstances[T2], argInstances3: ScalaCheckArgInstances[T3], argInstances4: ScalaCheckArgInstances[T4], argInstances5: ScalaCheckArgInstances[T5], argInstances6: ScalaCheckArgInstances[T6], argInstances7: ScalaCheckArgInstances[T7], argInstances8: ScalaCheckArgInstances[T8],
                                                                   prettyFreqMap: FreqMap[Set[Any]] => Pretty,
                                                                   asResult: AsResult[R],
                                                                   context: Option[Context],
                                                                   parameters: Parameters) extends ScalaCheckFunction {

  type SelfType = ScalaCheckFunction8[T1, T2, T3, T4, T5, T6, T7, T8, R]

  private implicit val asResult1  = asResult
  private implicit val (arb1,arb2,arb3,arb4,arb5,arb6,arb7,arb8) = (argInstances1.arbitrary,argInstances2.arbitrary,argInstances3.arbitrary,argInstances4.arbitrary,argInstances5.arbitrary,argInstances6.arbitrary,argInstances7.arbitrary,argInstances8.arbitrary)
  private implicit val (sh1,sh2,sh3,sh4,sh5,sh6,sh7,sh8) = (argInstances1.shrink,argInstances2.shrink,argInstances3.shrink,argInstances4.shrink,argInstances5.shrink,argInstances6.shrink,argInstances7.shrink,argInstances8.shrink)
  private implicit val (pr1,pr2,pr3,pr4,pr5,pr6,pr7,pr8) = (argInstances1.pretty,argInstances2.pretty,argInstances3.pretty,argInstances4.pretty,argInstances5.pretty,argInstances6.pretty,argInstances7.pretty,argInstances8.pretty)

  lazy val propFunction = (t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8) => {
    lazy val executed = execute(t1, t2, t3, t4, t5, t6, t7, t8)
    executeInContext(executed)
    argInstances1.collect(t1, argInstances2.collect(t2, argInstances3.collect(t3, argInstances4.collect(t4, argInstances5.collect(t5, argInstances6.collect(t6, argInstances7.collect(t7, argInstances8.collect(t8, asResultToProp(executed)))))))))
  }

  lazy val prop: Prop =
    makeProp((t1: T1) => makeProp((t2: T2) => makeProp((t3: T3) => makeProp((t4: T4) => makeProp((t5: T5) => makeProp((t6: T6) => makeProp((t7: T7) => makeProp((t8: T8) => propFunction(t1, t2, t3, t4, t5, t6, t7, t8), argInstances8.shrink), argInstances7.shrink), argInstances6.shrink), argInstances5.shrink), argInstances4.shrink), argInstances3.shrink), argInstances2.shrink), argInstances1.shrink)

  def noShrink: SelfType = copy(argInstances1 = argInstances1.copy(shrink = None), argInstances2 = argInstances2.copy(shrink = None), argInstances3 = argInstances3.copy(shrink = None), argInstances4 = argInstances4.copy(shrink = None), argInstances5 = argInstances5.copy(shrink = None), argInstances6 = argInstances6.copy(shrink = None), argInstances7 = argInstances7.copy(shrink = None), argInstances8 = argInstances8.copy(shrink = None))

  def setArbitrary1(a1: Arbitrary[T1]): SelfType = copy(argInstances1 = argInstances1.copy(arbitrary = a1))
  def setArbitrary2(a2: Arbitrary[T2]): SelfType = copy(argInstances2 = argInstances2.copy(arbitrary = a2))
  def setArbitrary3(a3: Arbitrary[T3]): SelfType = copy(argInstances3 = argInstances3.copy(arbitrary = a3))
  def setArbitrary4(a4: Arbitrary[T4]): SelfType = copy(argInstances4 = argInstances4.copy(arbitrary = a4))
  def setArbitrary5(a5: Arbitrary[T5]): SelfType = copy(argInstances5 = argInstances5.copy(arbitrary = a5))
  def setArbitrary6(a6: Arbitrary[T6]): SelfType = copy(argInstances6 = argInstances6.copy(arbitrary = a6))
  def setArbitrary7(a7: Arbitrary[T7]): SelfType = copy(argInstances7 = argInstances7.copy(arbitrary = a7))
  def setArbitrary8(a8: Arbitrary[T8]): SelfType = copy(argInstances8 = argInstances8.copy(arbitrary = a8))
  def setArbitraries(a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7], a8: Arbitrary[T8]): SelfType =
    setArbitrary1(a1).setArbitrary2(a2).setArbitrary3(a3).setArbitrary4(a4).setArbitrary5(a5).setArbitrary6(a6).setArbitrary7(a7).setArbitrary8(a8)

  def setGen1(g1: Gen[T1]): SelfType = setArbitrary1(Arbitrary(g1))
  def setGen2(g2: Gen[T2]): SelfType = setArbitrary2(Arbitrary(g2))
  def setGen3(g3: Gen[T3]): SelfType = setArbitrary3(Arbitrary(g3))
  def setGen4(g4: Gen[T4]): SelfType = setArbitrary4(Arbitrary(g4))
  def setGen5(g5: Gen[T5]): SelfType = setArbitrary5(Arbitrary(g5))
  def setGen6(g6: Gen[T6]): SelfType = setArbitrary6(Arbitrary(g6))
  def setGen7(g7: Gen[T7]): SelfType = setArbitrary7(Arbitrary(g7))
  def setGen8(g8: Gen[T8]): SelfType = setArbitrary8(Arbitrary(g8))
  def setGens(g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6], g7: Gen[T7], g8: Gen[T8]): SelfType =
    setGen1(g1).setGen2(g2).setGen3(g3).setGen4(g4).setGen5(g5).setGen6(g6).setGen7(g7).setGen8(g8)

  def setShrink1(s1: Shrink[T1]): SelfType = copy(argInstances1 = argInstances1.copy(shrink = Some(s1)))
  def setShrink2(s2: Shrink[T2]): SelfType = copy(argInstances2 = argInstances2.copy(shrink = Some(s2)))
  def setShrink3(s3: Shrink[T3]): SelfType = copy(argInstances3 = argInstances3.copy(shrink = Some(s3)))
  def setShrink4(s4: Shrink[T4]): SelfType = copy(argInstances4 = argInstances4.copy(shrink = Some(s4)))
  def setShrink5(s5: Shrink[T5]): SelfType = copy(argInstances5 = argInstances5.copy(shrink = Some(s5)))
  def setShrink6(s6: Shrink[T6]): SelfType = copy(argInstances6 = argInstances6.copy(shrink = Some(s6)))
  def setShrink7(s7: Shrink[T7]): SelfType = copy(argInstances7 = argInstances7.copy(shrink = Some(s7)))
  def setShrink8(s8: Shrink[T8]): SelfType = copy(argInstances8 = argInstances8.copy(shrink = Some(s8)))
  def setShrinks(s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4], s5: Shrink[T5], s6: Shrink[T6], s7: Shrink[T7], s8: Shrink[T8]): SelfType =
    setShrink1(s1).setShrink2(s2).setShrink3(s3).setShrink4(s4).setShrink5(s5).setShrink6(s6).setShrink7(s7).setShrink8(s8)

  def setPretty1(p1: T1 => Pretty): SelfType = copy(argInstances1 = argInstances1.copy(pretty = p1))
  def setPretty2(p2: T2 => Pretty): SelfType = copy(argInstances2 = argInstances2.copy(pretty = p2))
  def setPretty3(p3: T3 => Pretty): SelfType = copy(argInstances3 = argInstances3.copy(pretty = p3))
  def setPretty4(p4: T4 => Pretty): SelfType = copy(argInstances4 = argInstances4.copy(pretty = p4))
  def setPretty5(p5: T5 => Pretty): SelfType = copy(argInstances5 = argInstances5.copy(pretty = p5))
  def setPretty6(p6: T6 => Pretty): SelfType = copy(argInstances6 = argInstances6.copy(pretty = p6))
  def setPretty7(p7: T7 => Pretty): SelfType = copy(argInstances7 = argInstances7.copy(pretty = p7))
  def setPretty8(p8: T8 => Pretty): SelfType = copy(argInstances8 = argInstances8.copy(pretty = p8))
  def pretty1(p1: T1 => String): SelfType = setPretty1((t1: T1) => Pretty(_ => p1(t1)))
  def pretty2(p2: T2 => String): SelfType = setPretty2((t2: T2) => Pretty(_ => p2(t2)))
  def pretty3(p3: T3 => String): SelfType = setPretty3((t3: T3) => Pretty(_ => p3(t3)))
  def pretty4(p4: T4 => String): SelfType = setPretty4((t4: T4) => Pretty(_ => p4(t4)))
  def pretty5(p5: T5 => String): SelfType = setPretty5((t5: T5) => Pretty(_ => p5(t5)))
  def pretty6(p6: T6 => String): SelfType = setPretty6((t6: T6) => Pretty(_ => p6(t6)))
  def pretty7(p7: T7 => String): SelfType = setPretty7((t7: T7) => Pretty(_ => p7(t7)))
  def pretty8(p8: T8 => String): SelfType = setPretty8((t8: T8) => Pretty(_ => p8(t8)))

  def setPretties(p1: T1 => Pretty, p2: T2 => Pretty, p3: T3 => Pretty, p4: T4 => Pretty, p5: T5 => Pretty, p6: T6 => Pretty, p7: T7 => Pretty, p8: T8 => Pretty): SelfType =
    setPretty1(p1).setPretty2(p2).setPretty3(p3).setPretty4(p4).setPretty5(p5).setPretty6(p6).setPretty7(p7).setPretty8(p8)
  def pretties(p1: T1 => String, p2: T2 => String, p3: T3 => String, p4: T4 => String, p5: T5 => String, p6: T6 => String, p7: T7 => String, p8: T8 => String): SelfType =
    pretty1(p1).pretty2(p2).pretty3(p3).pretty4(p4).pretty5(p5).pretty6(p6).pretty7(p7).pretty8(p8)

  def setPrettyFreqMap(f: FreqMap[Set[Any]] => Pretty): SelfType =
    copy(prettyFreqMap = f)

  def collectArg1(f: T1 => Any): SelfType = copy(argInstances1 = argInstances1.copy(collectors = argInstances1.collectors :+ f))
  def collectArg2(f: T2 => Any): SelfType = copy(argInstances2 = argInstances2.copy(collectors = argInstances2.collectors :+ f))
  def collectArg3(f: T3 => Any): SelfType = copy(argInstances3 = argInstances3.copy(collectors = argInstances3.collectors :+ f))
  def collectArg4(f: T4 => Any): SelfType = copy(argInstances4 = argInstances4.copy(collectors = argInstances4.collectors :+ f))
  def collectArg5(f: T5 => Any): SelfType = copy(argInstances5 = argInstances5.copy(collectors = argInstances5.collectors :+ f))
  def collectArg6(f: T6 => Any): SelfType = copy(argInstances6 = argInstances6.copy(collectors = argInstances6.collectors :+ f))
  def collectArg7(f: T7 => Any): SelfType = copy(argInstances7 = argInstances7.copy(collectors = argInstances7.collectors :+ f))
  def collectArg8(f: T8 => Any): SelfType = copy(argInstances8 = argInstances8.copy(collectors = argInstances8.collectors :+ f))
  def collect1: SelfType = collectArg1(_.toString)
  def collect2: SelfType = collectArg2(_.toString)
  def collect3: SelfType = collectArg3(_.toString)
  def collect4: SelfType = collectArg4(_.toString)
  def collect5: SelfType = collectArg5(_.toString)
  def collect6: SelfType = collectArg6(_.toString)
  def collect7: SelfType = collectArg7(_.toString)
  def collect8: SelfType = collectArg8(_.toString)
  def collectAllArgs(f1: T1 => Any,f2: T2 => Any,f3: T3 => Any,f4: T4 => Any,f5: T5 => Any,f6: T6 => Any,f7: T7 => Any,f8: T8 => Any): SelfType =
    collectArg1(f1).collectArg2(f2).collectArg3(f3).collectArg4(f4).collectArg5(f5).collectArg6(f6).collectArg7(f7).collectArg8(f8)

  def collectAll: SelfType =
    collect1.collect2.collect3.collect4.collect5.collect6.collect7.collect8

  def prepare(action: (T1, T2, T3, T4, T5, T6, T7, T8) => (T1, T2, T3, T4, T5, T6, T7, T8)): SelfType =
    copy(execute = (t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8) => execute.tupled(action(t1, t2, t3, t4, t5, t6, t7, t8)))

  def setContext(context: Context): SelfType = copy(context = Some(context))

  def setParameters(ps: Parameters): SelfType = copy(parameters = ps)
}

case class ScalaCheckArgInstances[T](arbitrary: Arbitrary[T], shrink: Option[Shrink[T]], collectors: List[T => Any], pretty: T => Pretty) {
  def collect(t: T, p: Prop) =
    collectors.foldLeft(p)((res, cur) => Prop.collect(t)(p))
}

object ScalaCheckProperty {

  implicit def ScalaCheckPropertyAsExecution[S <: ScalaCheckProperty]: AsExecution[S] = new AsExecution[S] {
    def execute(s: => S): Execution =
      Execution.withEnv(env => AsResultProp.check(s.prop, s.parameters.overrideWith(env.commandLine), s.prettyFreqMap))
  }

  def makeProp[T](f: T => Prop, shrink: Option[Shrink[T]])(implicit a: Arbitrary[T], p: T => Pretty): Prop =
    shrink.fold(Prop.forAllNoShrink(f))(s => Prop.forAll(f)(identity, a, s, p))

  def TNList(n: Int) = (1 to n).map("T"+_).mkString(", ")
  def TNParamList(n: Int) = (1 to n).map(i => s"t$i: T$i").mkString(", ")
  def NParamList(n: Int) = (1 to n).map(i => s"t$i").mkString(", ")

  def allScalaCheckFunctionN(n: Int): String =
    (2 to n).map(scalaCheckFunctionN).mkString("\n\n")


  def scalaCheckFunctionN(n: Int): String =
  s"""
case class ScalaCheckFunction$n[${TNList(n)}, R](
  execute: (${TNList(n)}) => R,
  ${(1 to n).map(i => s"argInstances$i: ScalaCheckArgInstances[T$i]").mkString(", ") },
  prettyFreqMap: FreqMap[Set[Any]] => Pretty,
  asResult: AsResult[R],
  context: Option[Context],
  parameters: Parameters) extends ScalaCheckFunction {

  type SelfType = ScalaCheckFunction$n[${TNList(n)}, R]

  private implicit val asResult1  = asResult
  private implicit val ${ (1 to n).map(i => s"arb$i").mkString("(", ",", ")") } = ${ (1 to n).map(i => s"argInstances$i.arbitrary").mkString("(", ",", ")") }
  private implicit val ${ (1 to n).map(i => s"sh$i").mkString("(", ",", ")") } = ${ (1 to n).map(i => s"argInstances$i.shrink").mkString("(", ",", ")") }
  private implicit val ${ (1 to n).map(i => s"pr$i").mkString("(", ",", ")") } = ${ (1 to n).map(i => s"argInstances$i.pretty").mkString("(", ",", ")") }

  lazy val propFunction = (${TNParamList(n)}) => {
    lazy val executed = execute(${NParamList(n)})
    executeInContext(executed)
    ${(1 to n).reverse.foldLeft("asResultToProp(executed)"){ (res, i) => s"argInstances$i.collect(t$i, $res)"}}
  }

  lazy val prop: Prop =
    ${(1 to n).reverse.foldLeft(s"propFunction(${NParamList(n)})") { (res, i) =>
       s"makeProp((t$i: T$i) => $res, argInstances$i.shrink)"
    }}

  def noShrink: SelfType = copy(${(1 to n).map(i => s"argInstances$i = argInstances$i.copy(shrink = None)").mkString(", ")})

  ${(1 to n).map(i => s"def setArbitrary$i(a$i: Arbitrary[T$i]): SelfType = copy(argInstances$i = argInstances$i.copy(arbitrary = a$i))").mkString("\n  ")}
  def setArbitraries(${(1 to n).map(i => s"a$i: Arbitrary[T$i]").mkString(", ")}): SelfType =
    ${(1 to n).map(i => s"setArbitrary$i(a$i)").mkString(".")}

  ${(1 to n).map(i => s"def setGen$i(g$i: Gen[T$i]): SelfType = setArbitrary$i(Arbitrary(g$i))").mkString("\n  ")}
  def setGens(${(1 to n).map(i => s"g$i: Gen[T$i]").mkString(", ")}): SelfType =
    ${(1 to n).map(i => s"setGen$i(g$i)").mkString(".")}

  ${(1 to n).map(i => s"def setShrink$i(s$i: Shrink[T$i]): SelfType = copy(argInstances$i = argInstances$i.copy(shrink = Some(s$i)))").mkString("\n  ")}
  def setShrinks(${(1 to n).map(i => s"s$i: Shrink[T$i]").mkString(", ")}): SelfType =
    ${(1 to n).map(i => s"setShrink$i(s$i)").mkString(".")}

  ${(1 to n).map(i => s"def setPretty$i(p$i: T$i => Pretty): SelfType = copy(argInstances$i = argInstances$i.copy(pretty = p$i))").mkString("\n  ")}
  ${(1 to n).map(i => s"def pretty$i(p$i: T$i => String): SelfType = setPretty$i((t$i: T$i) => Pretty(_ => p$i(t$i)))").mkString("\n  ")}

  def setPretties(${(1 to n).map(i => s"p$i: T$i => Pretty").mkString(", ")}): SelfType =
    ${(1 to n).map(i => s"setPretty$i(p$i)").mkString(".")}
  def pretties(${(1 to n).map(i => s"p$i: T$i => String").mkString(", ")}): SelfType =
    ${(1 to n).map(i => s"pretty$i(p$i)").mkString(".")}

  def setPrettyFreqMap(f: FreqMap[Set[Any]] => Pretty): SelfType =
    copy(prettyFreqMap = f)

  ${(1 to n).map(i => s"def collectArg$i(f: T$i => Any): SelfType = copy(argInstances$i = argInstances$i.copy(collectors = argInstances$i.collectors :+ f))").mkString("\n  ")}
  ${(1 to n).map(i => s"def collect$i: SelfType = collectArg$i(_.toString)").mkString("\n  ")}
  def collectAllArgs(${(1 to n).map(i => s"f$i: T$i => Any").mkString(",")}): SelfType =
    ${(1 to n).map(i => s"collectArg$i(f$i)").mkString(".")}

  def collectAll: SelfType =
    ${(1 to n).map(i => s"collect$i").mkString(".")}

  def prepare(action: (${TNList(n)}) => (${TNList(n)})): SelfType =
    copy(execute = (${TNParamList(n)}) => execute.tupled(action(${NParamList(n)})))

  def setContext(context: Context): SelfType = copy(context = Some(context))

  def setParameters(ps: Parameters): SelfType = copy(parameters = ps)
}""".stripMargin


}
