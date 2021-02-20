package org.specs2
package mutable

import matcher.*
import control.Debug
import AnyMatchers.*
import execute.*
import ThrownExpectationsSpecData.*

class ThrownExpectationsSpec extends Spec with ResultMatchers:

  "An acceptance spec using" >> {
    "ThrownExpectations should fail when the first must matcher fails in an Example" >> {
      execute(body1) `must` beFailing
    }
    "ThrownExpectations should fail when the first should matcher fails in an Example" >> {
      execute(body2) `must` beFailing
    }
    "MustThrownExpectations should fail when the first matcher fails in an Example" >> {
      execute(body3) `must` beFailing
    }
    "ShouldThrownExpectations should fail when the first matcher fails in an Example" >> {
      execute(body4) `must` beFailing
    }
  }
  "If a DataTable fails it must throw a DecoratedResultException containing the table data" >> {
    execute(body5) `must` beLike { case DecoratedResult(_, Failure(_,_,_,_)) => ok }
  }

  "Results must only be checked once" >> {
    val body = body6
    execute(body)
    body.getCallsNb `must` be_==(1)
  }

  def execute[T](t: =>T) = ResultExecution.execute(t)(_ => Success())
  
object ThrownExpectationsSpecData:
  def body1 = new MustThrownExpectations {
    1 `must` beEqualTo(2); success
  }
  def body2 = new ShouldThrownExpectations {
    1 `should` beEqualTo(2); success
  }
  def body3 = new MustThrownExpectations {
    1 `must` beEqualTo(2); success
  }
  def body4 = new ShouldThrownExpectations {
    1 `should` beEqualTo(2); success
  }
  def body5 = new MustThrownExpectations with DataTables with Debug {
    "a" | "b" | "c" |>
    1   ! 1   ! 2   |
    1   ! 1   ! 3   | { (a, b, c) => (a+b) `must` beEqualTo(c) }
  }

  def body6 = new Body6 {}

  trait Body6 extends MustThrownExpectations:
    var i = 0
    checkResultFailure { i += 1; success }

    def getCallsNb: Int = i
