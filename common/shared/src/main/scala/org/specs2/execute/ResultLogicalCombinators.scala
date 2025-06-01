package org.specs2
package execute

import text.Message.*

/** This trait provides logical combinators for Booleans and Results: and, or, not
  *
  * Note that a Result expression can throw an exception which will be changed into a failure or an error before being
  * used with and/or/not
  *
  * The implicit definitions can not be easily replaced with extension methods otherwise this brings too many conflicts
  * with the synthetized method names, like extension_and for both the extension for a Boolean value and a Result
  */
trait ResultLogicalCombinators extends Results:

  implicit def combineBoolean(b: =>Boolean): ResultLogicalCombinator =
    new ResultLogicalCombinator(Results.toResult(b))

  implicit def combineResult(r: =>Result): ResultLogicalCombinator =
    new ResultLogicalCombinator(r)

  class ResultLogicalCombinator(res: =>Result):
    private lazy val result = ResultExecution.executeThrowable(res)
    private lazy val r = result match
      case Left(r1)  => r1
      case Right(r1) => r1

    /** @return
      *   the logical and combination of 2 results
      */
    infix def and(other: =>Result): Result =
      lazy val o = ResultExecution.execute(other)
      def combine(result: Result, ifFail: Failure => Failure = identity, ifError: Error => Error = identity) =
        result match
          case s @ Success(_, _) =>
            o match
              case Success(m, e) =>
                if r.message == m || r.message.isEmpty then
                  Success(m, concat(s.exp, e), r.expectationsNb + o.expectationsNb)
                else Success(r.message + " and " + m, concat(s.exp, e), r.expectationsNb + o.expectationsNb)
              case DecoratedResult(d, r1) =>
                DecoratedResult(d, r.and(r1))
              case Failure(_, _, _, _) | Error(_, _) =>
                o.addExpectationsNb(r.expectationsNb).mapExpected((e: String) => concat(r.expected, e))
              case _ =>
                r.addExpectationsNb(o.expectationsNb).mapExpected((e: String) => concat(e, o.expected))

          case Pending(_) | Skipped(_, _) =>
            o match
              case s @ Success(_, _)       => s
              case f @ Failure(_, _, _, _) => f
              case e @ Error(_, _)         => e
              case DecoratedResult(d, r1)  => DecoratedResult(d, r.and(r1))
              case _                       => result

          case d @ DecoratedResult(_, _) =>
            o match
              case DecoratedResult(d2, r2) =>
                val andResult = d.result and r2
                if andResult.isSuccess then DecoratedResult(d.decorator, andResult)
                else DecoratedResult(d2, andResult)
              case another => DecoratedResult(d.decorator, d.result and another)

          case f @ Failure(_, _, _, _) => ifFail(f.addExpectationsNb(1)) // re-throw if necessary
          case e @ Error(_, _)         => ifError(e.addExpectationsNb(1)) // re-throw if necessary
      // if the original result threw an exception we must re-throw it
      result match
        case Left(r1) =>
          combine(r1, (f: Failure) => throw new FailureException(f), (e: Error) => throw new ErrorException(e))
        case Right(r1) => combine(r1)

    /** @return
      *   the logical or combination of 2 results
      */
    infix def or(other: =>Result): Result =
      lazy val o = ResultExecution.execute(other)

      r match
        case Success(_, _)           => r.addExpectationsNb(1)
        case f @ Failure(_, _, _, _) =>
          o match
            case s @ Success(m, exp) =>
              if r.message == m then Success(m, exp, r.expectationsNb + s.expectationsNb)
              else Success(r.message + " and " + m, exp, r.expectationsNb + s.expectationsNb)
            case Failure(m, e, st, d) =>
              Failure(r.message + " and " + m, e, f.stackTrace ::: st, d).addExpectationsNb(r.expectationsNb)
            case DecoratedResult(d, r1) =>
              DecoratedResult(d, r.or(r1))
            case _ =>
              r.addExpectationsNb(o.expectationsNb).mapExpected((e: String) => concat(e, o.expected))

        case Pending(_) | Skipped(_, _) =>
          o match
            case s @ Success(_, _)       => s
            case f @ Failure(_, _, _, _) => f
            case DecoratedResult(d, r1)  => DecoratedResult(d, r.or(r1))
            case _                       => o

        case d @ DecoratedResult(_, _) =>
          o match
            case DecoratedResult(d2, r2) =>
              val orResult = d.result or r2
              if orResult.isSuccess then DecoratedResult(d.decorator, orResult)
              else DecoratedResult(d2, orResult)
            case other1 => DecoratedResult(d.decorator, d.result or other1)
        case Error(_, _) => other

    /** @return
      *   Success if it is a failure and vice-versa
      */
    def not: Result =
      r.negate

    /** only consider this result if the condition is true */
    def when(condition: Boolean, message: String = ""): Result = if condition then res else Success(message)

    /** only consider this result if the condition is false */
    def unless(condition: Boolean, message: String = ""): Result = res.when(!condition, message)

    /** when the condition is true the result it taken as is, when it's false, take its negation */
    def iff(condition: Boolean): Result = if condition then res else res.not

object ResultLogicalCombinators extends ResultLogicalCombinators
