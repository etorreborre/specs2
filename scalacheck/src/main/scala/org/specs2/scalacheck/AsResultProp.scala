package org.specs2
package scalacheck

import org.scalacheck.{Properties, Gen, Prop}
import org.scalacheck.util.*
import execute.*
import scala.annotation.tailrec

/** Implicits to convert Prop to AsResult and AsResult to Prop
  */
trait AsResultProp extends ScalaCheckPropertyCheck with AsResultPropLowImplicits:

  given asResultToProp[R: AsResult]: Conversion[R, Prop] with
    def apply(r: R): Prop =
      r.asInstanceOf[Matchable] match
        case p: Prop => p
        case _ =>
          Prop.apply { (params: Gen.Parameters) =>
            lazy val result = ResultExecution.execute(AsResult(r))

            @tailrec
            def resultToProp(r: execute.Result): Prop =
              r match
                case f: execute.Failure => Prop.exception(new FailureException(f))
                case s: execute.Skipped => Prop.exception(new SkipException(s))
                case p: execute.Pending => Prop.exception(new PendingException(p))
                case e: execute.Error   => Prop.exception(e.exception)

                case execute.DecoratedResult(_, r1) =>
                  // display the datatables on a new line
                  resultToProp(r1.updateMessage("\n" + _))

                case other => Prop.passed

            val prop = resultToProp(result)

            result match
              case f: execute.Failure if f.details != NoDetails =>
                prop.apply(params).collect(f.details)

              case _ =>
                prop.apply(params)
          }

  /** implicit typeclass instance to create examples from a Prop */
  given propAsResult(using p: Parameters, pfq: FreqMap[Set[Any]] => Pretty): AsResult[Prop] with
    def asResult(prop: =>Prop): Result =
      check(prop, p, pfq)

trait AsResultPropLowImplicits extends ScalaCheckPropertyCheck with ScalaCheckParameters:
  /** implicit typeclass instance to create examples from Properties */
  given propertiesAsResult(using p: Parameters, pfq: FreqMap[Set[Any]] => Pretty): AsResult[Properties] with
    def asResult(properties: =>Properties): Result =
      checkProperties(properties, p, pfq)

  given scalaCheckPropertyAsResult[S <: ScalaCheckProperty]: AsResult[S] with
    def asResult(prop: =>S): Result =
      try
        lazy val p = prop
        check(p.prop, p.parameters, p.prettyFreqMap)
      catch
        // this is necessary in case of thrown expectations inside the property
        case FailureException(f) =>
          f

        case t: Throwable =>
          AsResultProp.propAsResult.asResult(Prop.exception(t))

object AsResultProp extends AsResultProp
