package org.specs2
package data

import matcher.{ResultMatchers, ThrownExpectations, ProcessMatchers}
import specification.Groups
import specification.script.Specification
import scalaz.stream.Process._
import ProcessMatchers._

class ProcessesSpec extends Specification with Groups with ResultMatchers with ThrownExpectations { def is = s2"""

 Streams
 =======

 + withNext
 + withPreviousAndNext

"""

  "streams" - new group {
    eg := {
      val p1 = emitAll(Seq(1, 2, 3)).toSource
      val p2 = p1 |> Processes.withNext

      val next = new scala.collection.mutable.ListBuffer[Option[Int]]
      def process = {
        def go: Process1[(Int, Option[Int]), Int] = {
          receive1 {
            case (i, n) =>
              next.append(n)
              emit(i) fby go
          }
        }
        go
      }

      (p2 |> process) must haveLog(1, 2, 3)
      next.toList must_== List(Some(2), Some(3), None)
    }

    eg := {
      val p1 = emitAll(Seq(1, 2, 3)).toSource
      val p2 = p1 |> Processes.withPreviousAndNext

      val previous = new scala.collection.mutable.ListBuffer[Option[Int]]
      val next = new scala.collection.mutable.ListBuffer[Option[Int]]
      def process = {
        def go: Process1[(Option[Int], Int, Option[Int]), Int] = {
          receive1 {
            case (p, i, n) =>
              previous.append(p)
              next.append(n)
              emit(i).filter(_ => i % 2 == 0) fby go
          }
        }
        go
      }

      (p2 |> process).runLog.run must_== List(2)
      previous.toList must_== List(None, Some(1), Some(2))
      next.toList must_== List(Some(2), Some(3), None)
    }
  }

}
