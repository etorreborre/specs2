package org.specs2
package matcher

import org.scalacheck.{Gen, Prop}

trait ResultPropertyImplicits {

  implicit def unitToProp(u: =>Unit): Prop = booleanToProp({u; true})

  /** @return a Prop that will not throw an exception when evaluated */
  implicit def propToProp(p: =>Prop): Prop = new Prop {
    def apply(params: Gen.Parameters) = {
      try p(params)
      catch {
        case execute.FailureException(f) if f.details != execute.NoDetails =>
          (Prop.falsified :| (f.message+" ("+f.location+")"))(params).collect(f.details)

        case execute.FailureException(f) =>
          (Prop.falsified :| (f.message+" ("+f.location+")"))(params)

        case e: Throwable => Prop.exception(e)(params)
      }
    }
  }

  implicit def booleanToProp(b: =>Boolean): Prop = resultProp(if (b) execute.Success() else execute.Failure())
  implicit def callByNameMatchResultToProp[T](m: =>MatchResult[T]): Prop = resultProp(m.toResult)
  implicit def matchResultToProp[T](m: MatchResult[T]): Prop = resultProp(m.toResult)

  implicit def resultProp(r: =>execute.Result): Prop = {
    new Prop {
      def apply(params: Gen.Parameters) = {
        lazy val result = execute.ResultExecution.execute(r)
        val prop =
          result match {
            case f : execute.Failure => Prop.falsified :| (f.message+" ("+f.location+")")
            case s : execute.Skipped => Prop.exception(new execute.SkipException(s))
            case p : execute.Pending => Prop.exception(new execute.PendingException(p))
            case e : execute.Error   => Prop.exception(e.exception)
            case other               => Prop.passed
          }
        result match {
          case f: execute.Failure if f.details != execute.NoDetails =>
            prop.apply(params).collect(f.details)

          case _ =>
            prop.apply(params)
        }
      }
    }
  }
}
