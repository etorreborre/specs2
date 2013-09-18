package org.specs2
package execute

import text.Message._
import ResultLogicalCombinators._
import control.Exceptions._

/**
 * This trait provides logical combinators to Results: and, or, not
 *
 * A Result expression can throw an exception which will be changed into a failure or an error before being used
 * with and/or/not
 */
private[specs2]
trait ResultLogicalCombinators extends Results {

  implicit def combineBoolean(b: =>Boolean): ResultLogicalCombinator = new ResultLogicalCombinator(b)
  implicit def combineResult(r: =>Result)  : ResultLogicalCombinator = new ResultLogicalCombinator(r)

  class ResultLogicalCombinator(res: =>Result) {
    private val result = ResultExecution.executeThrowable(res)
    private val r = result match {
      case Left(r1)  => r1
      case Right(r1) => r1
    }
    /**
     * @return the logical and combination of 2 results
     */
    def and(other: =>Result): Result = {
      lazy val o = ResultExecution.execute(other)
      def combine(result: Result, ifFail: Failure => Failure = identity, ifError: Error => Error = identity) = {
        result match {
          case s @ Success(_,_) =>   {
            o match {
              case Success(m, e)                  => if (r.message == m || r.message.isEmpty) Success(m, concat(s.exp, e), r.expectationsNb + o.expectationsNb)
              else                                     Success(r.message+" and "+m, concat(s.exp, e), r.expectationsNb + o.expectationsNb)
              case DecoratedResult(d, r1)         => DecoratedResult(d, r.and(r1))
              case Failure(_,_,_,_) | Error(_,_)  => o.addExpectationsNb(r.expectationsNb).mapExpected((e: String) => concat(r.expected, e))
              case _                              => r.addExpectationsNb(o.expectationsNb).mapExpected((e: String) => concat(e, o.expected))
            }
          }
          case Pending(_) | Skipped(_,_)     => {
            o match {
              case s @ Success(_,_)       => s
              case f @ Failure(_,_,_,_)   => f
              case e @ Error(_,_)         => e
              case DecoratedResult(d, r1) => DecoratedResult(d, r.and(r1))
              case _                      => result
            }
          }
          case d @ DecoratedResult(_,_)     => {
            o match {
              case DecoratedResult(d2, r2) => {
                val andResult = (d.result and r2)
                if (andResult.isSuccess) DecoratedResult(d.decorator, andResult)
                else                     DecoratedResult(d2, andResult)
              }
              case another                 => DecoratedResult(d.decorator, d.result and another)
            }
          }
          case f @ Failure(_,_,_,_)        => ifFail(f) // re-throw if necessary
          case e @ Error(_,_)              => ifError(e) // re-throw if necessary
        }
      }
      // if the original result threw an exception we must re-throw it
      result match {
        case Left(r1)  => combine(r1, (f:Failure) => throw new FailureException(f), (e:Error) => throw new ErrorException(e))
        case Right(r1) => combine(r1)
      }
    }
    /**
     * @return the logical or combination of 2 results
     */
    def or(other: =>Result): Result = {
      lazy val o = ResultExecution.execute(other)
      r match {
        case Success(_,_)         => r
        case f @ Failure(_,_,_,_) => {
          o match {
            case s @ Success(m, exp)    => if (r.message == m) r.addExpectationsNb(s.expectationsNb)
            else Success(r.message+" and "+m, exp, r.expectationsNb + s.expectationsNb)
            case Failure(m, e, st, d)   => Failure(r.message+" and "+m, e, f.stackTrace ::: st, d).addExpectationsNb(r.expectationsNb)
            case DecoratedResult(d, r1) => DecoratedResult(d, r.or(r1))
            case _                      => r.addExpectationsNb(o.expectationsNb).mapExpected((e: String) => concat(e, o.expected))
          }
        }
        case Pending(_) | Skipped(_,_)     => {
          o match {
            case s @ Success(_,_)       => s
            case f @ Failure(_,_,_,_)   => f
            case DecoratedResult(d, r1) => DecoratedResult(d, r.or(r1))
            case _                      => o
          }
        }
        case d @ DecoratedResult(_,_)     => {
          o match {
            case DecoratedResult(d2, r2) => {
              val orResult = (d.result or r2)
              if (orResult.isSuccess) DecoratedResult(d.decorator, orResult)
              else                    DecoratedResult(d2, orResult)
            }
            case other1                   => DecoratedResult(d.decorator, d.result or other1)
          }
        }
        case Error(_, _) => other
      }
    }

    /**
     * @return Success if it is a failure and vice-versa
     */
    def not: Result = r match {
      case Success(m,e)     => Failure(m, e)
      case Failure(m,e,_,_) => Success(m)
      case other            => other
    }

    /** only consider this result if the condition is true */
    def when(b: Boolean, m: String= ""): Result= if (b) res else Success(m)
    /** only consider this result if the condition is false */
    def unless(b: Boolean, m: String= ""): Result = res.when(!b, m)
    /** when the condition is true the result it taken as is, when it's false, take its negation */
    def iff(b: Boolean): Result = if (b) res else res.not
  }
}

object ResultLogicalCombinators extends ResultLogicalCombinators
