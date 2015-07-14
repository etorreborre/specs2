package org.specs2
package scalacheck

import org.scalacheck.{Gen, Prop}
import org.scalacheck.util._
import execute._
import org.specs2.main.{CommandLine, CommandLineAsResult}

/**
 * Implicits to convert Prop to AsResult and AsResult to Prop
 */
trait AsResultProp extends ScalaCheckPropertyCheck with ScalaCheckParameters with AsResultPropLowImplicits {

  implicit def asResultToProp[R : AsResult](r: R): Prop = {
    r match {
      case p: Prop => p
      case _ =>
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
  }

  implicit def scalaCheckPropertyCommandLineAsResult[S <: ScalaCheckProperty]: CommandLineAsResult[S] = new CommandLineAsResult[S] {
    def asResult(commandLine: CommandLine, prop: =>S): Result = {
      lazy val p = prop
      check(p.prop, p.parameters.overrideWith(commandLine), p.prettyFreqMap)
    }
  }

  /** implicit typeclass instance to create examples from Props */
  implicit def propAsResult[P <: Prop](implicit p: Parameters, pfq: FreqMap[Set[Any]] => Pretty): AsResult[P] = new AsResult[P] {
    def asResult(prop: =>P): Result =
      check(prop, p, pfq)
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
