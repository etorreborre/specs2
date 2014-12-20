package org.specs2
package scalacheck

import org.scalacheck.{Gen, Prop}
import execute._

/**
 * Implicits to convert Prop to AsResult and AsResult to Prop
 */
trait AsResultProp extends ScalaCheckPropertyCheck with ScalaCheckParameters2 {

  implicit def asResultToProp[R : AsResult](r: R): Prop = {
    new Prop {
      def apply(params: Gen.Parameters) = {
        lazy val result = ResultExecution.execute(AsResult(r))

        val prop =
          result match {
            case f : execute.Failure => Prop.falsified :| (f.message+" ("+f.location+")")
            case s : execute.Skipped => Prop.exception(new SkipException(s))
            case p : execute.Pending => Prop.exception(new PendingException(p))
            case e : execute.Error   => Prop.exception(e.exception)
            case other               => Prop.passed
          }
        result match {
          case f: execute.Failure if f.details != NoDetails =>
            prop.apply(params).collect(f.details)

          case _ =>
            prop.apply(params)
        }
      }
    }
  }

  implicit def scalaCheckPropAsResult[T, R]: AsResult[ScalaCheckProp[T, R]] = new AsResult[ScalaCheckProp[T, R]] {
    def asResult(prop: =>ScalaCheckProp[T, R]): Result = check(prop.prop, prop.parameters)
  }

  implicit def scalaCheckProp2AsResult[T1, T2, R]: AsResult[ScalaCheckProp2[T1, T2, R]] = new AsResult[ScalaCheckProp2[T1, T2, R]] {
    def asResult(prop: =>ScalaCheckProp2[T1, T2, R]): Result = check(prop.prop, prop.parameters)
  }

  /** implicit typeclass instance to create examples from Props */
  implicit def propAsResult[P <: Prop](implicit p: Parameters): AsResult[P] = new AsResult[P] {
    def asResult(prop: =>P): Result = check(prop, p)
  }

}

object AsResultProp extends AsResultProp
