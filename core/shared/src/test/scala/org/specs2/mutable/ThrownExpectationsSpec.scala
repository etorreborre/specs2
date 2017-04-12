package org.specs2
package mutable

import matcher._
import control.Debug
import execute._
import ThrownExpectationsSpecData._

class ThrownExpectationsSpec extends Spec with ResultMatchers {

  "An acceptance spec using" >> {
    "ThrownExpectations should fail when the first must matcher fails in an Example" in {
      execute(body1) must beFailing
    }
    "ThrownExpectations should fail when the first should matcher fails in an Example" in {
      execute(body2) must beFailing
    }
    "MustThrownExpectations should fail when the first matcher fails in an Example" in {
      execute(body3) must beFailing
    }
    "ShouldThrownExpectations should fail when the first matcher fails in an Example" in {
      execute(body4) must beFailing
    }
  }
  "If a DataTable fails it must throw a DecoratedResultException containing the table data" in {
    execute(body5) must beLike { case DecoratedResult(_, Failure(_,_,_,_)) => ok }
  }

  "Results must only be checked once" in {
    val body = body6
    execute(body)
    body.i must be_==(1)
  }

  def execute[T](t: =>T) = ResultExecution.execute(t)(_ => Success())
}
object ThrownExpectationsSpecData {
  def body1 = new MustThrownExpectations with matcher.Scope {
    1 must_== 2; success
  }
  def body2 = new ShouldThrownExpectations with matcher.Scope {
    1 should_== 2; success
  }
  def body3 = new MustThrownExpectations with matcher.Scope {
    1 must_== 2; success
  }
  def body4 = new ShouldThrownExpectations with matcher.Scope {
    1 should_== 2; success
  }
  def body5 = new MustThrownExpectations with matcher.Scope with DataTables with Debug {
    "a" | "b" | "c" |>
    1   ! 1   ! 2   |
    1   ! 1   ! 3   | { (a, b, c) => (a+b) must_== c }
  }
  def body6 = new MustThrownExpectations with matcher.Scope {
    var i = 0
    checkResultFailure { i += 1; success }
  }

}
