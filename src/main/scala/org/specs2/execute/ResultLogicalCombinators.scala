package org.specs2
package execute

import text.Message._
import ResultLogicalCombinators._

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
    private val r = ResultExecution.execute(res)
    /**
     * @return the logical and combination of 2 results
     */
    def and(other: =>Result): Result = r match {
      case s @ Success(_,_) =>   {
        val o = other
          o match {
            case Success(m, e)                  => if (r.message == m || r.message.isEmpty) Success(m, concat(s.exp, e), r.expectationsNb + o.expectationsNb)
                                                   else                                     Success(r.message+" and "+m, concat(s.exp, e), r.expectationsNb + o.expectationsNb)
            case DecoratedResult(d, r1)         => DecoratedResult(d, r.and(r1))
            case Failure(_,_,_,_) | Error(_,_)  => o.addExpectationsNb(r.expectationsNb).mapExpected((e: String) => concat(r.expected, e))
            case _                              => r.addExpectationsNb(o.expectationsNb).mapExpected((e: String) => concat(o.expected, e))
          }
      }
      case Pending(_) | Skipped(_,_)     => {
        val o = other
        o match {
          case s @ Success(_,_)       => s
          case f @ Failure(_,_,_,_)   => f
          case e @ Error(_,_)         => e
          case DecoratedResult(d, r1) => DecoratedResult(d, r.and(r1))
          case _                      => o
        }
      }
      case d @ DecoratedResult(_,_)     => {
        other match {
          case DecoratedResult(d2, r2) => {
            val andResult = (d.result and r2)
            if (andResult.isSuccess) DecoratedResult(d.decorator, andResult)
            else                     DecoratedResult(d2, andResult)
          }
          case o                   => DecoratedResult(d.decorator, d.result and o)
        }
      }
      case _                          => r
    }
    /**
     * @return the logical or combination of 2 results
     */
    def or(other: =>Result): Result = r match {
      case f @ Failure(_,_,_,_) => {
        val o = other
        o match {
          case s @ Success(m, exp)    => if (r.message == m) r.addExpectationsNb(s.expectationsNb)
                                         else Success(r.message+" and "+m, exp, r.expectationsNb + s.expectationsNb)
          case Failure(m, e, st, d)   => Failure(r.message+" and "+m, e, f.stackTrace ::: st, d).addExpectationsNb(r.expectationsNb)
          case DecoratedResult(d, r1) => DecoratedResult(d, r.or(r1))
          case _                      => r.addExpectationsNb(o.expectationsNb).mapExpected((e: String) => concat(o.expected, e))
        }
      }
      case Pending(_) | Skipped(_,_)     => {
        val o = other
        o match {
          case s @ Success(_,_)       => s
          case f @ Failure(_,_,_,_)   => f
          case DecoratedResult(d, r1) => DecoratedResult(d, r.or(r1))
          case _                      => o
        }
      }
      case d @ DecoratedResult(_,_)     => {
        other match {
          case DecoratedResult(d2, r2) => {
            val orResult = (d.result or r2)
            if (orResult.isSuccess) DecoratedResult(d.decorator, orResult)
            else                     DecoratedResult(d2, orResult)
          }
          case o                   => DecoratedResult(d.decorator, d.result or o)
        }
      }
      case _ => r.addExpectationsNb(other.expectationsNb)
    }

    /**
     * @return Success if it is a failure and vice-versa
     */
    def not: Result = r match {
      case Success(m,e)     => Failure(m, e)
      case Failure(m,e,_,_) => Success(m)
      case other            => other
    }
  }
}

object ResultLogicalCombinators extends ResultLogicalCombinators
