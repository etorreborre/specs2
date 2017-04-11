package org.specs2
package scalacheck

import org.scalacheck.{Properties, Gen, Prop}
import org.scalacheck.util._
import execute._
import scala.annotation.tailrec

/**
 * Implicits to convert Prop to AsResult and AsResult to Prop
 */
trait AsResultProp extends ScalaCheckPropertyCheck with ScalaCheckParameters with AsResultPropLowImplicits {

  implicit def asResultToProp[R : AsResult](r: R): Prop = {
    r match {
      case p: Prop => p
      case _ =>
        Prop.apply { params: Gen.Parameters =>
          lazy val result = ResultExecution.execute(AsResult(r))

          @tailrec
          def resultToProp(r: execute.Result): Prop =
            r match {
              case f : execute.Failure            => Prop.exception(new FailureException(f))
              case s : execute.Skipped            => Prop.exception(new SkipException(s))
              case p : execute.Pending            => Prop.exception(new PendingException(p))
              case e : execute.Error              => Prop.exception(e.exception)
              case execute.DecoratedResult(_, r1) => resultToProp(r1)
              case other                          => Prop.passed
            }

          val prop = resultToProp(result)

          result match {
            case f: execute.Failure if f.details != NoDetails =>
              prop.apply(params).collect(f.details)

            case _ =>
              prop.apply(params)
          }
        }
     }
  }

  /** implicit typeclass instance to create examples from a Prop */
  implicit def propAsResult(implicit p: Parameters, pfq: FreqMap[Set[Any]] => Pretty): AsResult[Prop] = new AsResult[Prop] {
    def asResult(prop: =>Prop): Result =
      check(prop, p, pfq)
  }

  /** implicit typeclass instance to create examples from Properties */
  implicit def propertiesAsResult(implicit p: Parameters, pfq: FreqMap[Set[Any]] => Pretty): AsResult[Properties] = new AsResult[Properties] {
    def asResult(properties: =>Properties): Result =
      checkProperties(properties, p, pfq)
  }
}

trait AsResultPropLowImplicits extends ScalaCheckPropertyCheck {
  implicit def scalaCheckPropertyAsResult[S <: ScalaCheckProperty]: AsResult[S] = new AsResult[S] {
    def asResult(prop: =>S): Result = {
      lazy val p = prop
      check(p.prop, p.parameters, p.prettyFreqMap)
    }
  }
}

object AsResultProp extends AsResultProp
