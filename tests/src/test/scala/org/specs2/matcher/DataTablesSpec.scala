package org.specs2
package matcher

import java.util.concurrent.{ExecutorService}
import org.specs2.concurrent.ExecutionEnv
import scalaz.concurrent.Future
import sys._
import execute._

class DataTablesSpec extends Specification with DataTables with ResultMatchers { def is = s2"""

 DataTables are useful to specify lots of examples varying just by a few values.

  A simple table can be declared to specify the addition on Integers                                         $e1
  A table is not executed if there is no 'play' sign `|>` declared on it                                     $e2
  If there are failures on rows they must be reported                                                        $e3
  If there is an exception on any row, it will not stop the example                                          $e4
  If the first value is a string, !! can be used as a cell separator                                         $e5
  !! can be used as a cell separator with any type                                                           $e6
  A table can be built with just one column                                                                  $e7
  A table must work with values of different subtypes of the first row                                       $e8
  A table must work ok in a mutable spec                                                                     $e9
  A table must be formatted with equal-length cells
    when succeeding                                                                                          $e10
    when failing                                                                                             $e11
  2 tables results can be and-ed together                                                                    $e12
  a cell can have null values                                                                                $e13

  It is possible to format the cells
    for an immutable spec                      $format1
    for a  mutable spec                        $format2

 ${section("travis")}

 Applicative style
 =================

 The default execution model for DataTables is sequential. You can however execute the rows of a DataTable
   by using `|*` and the execution function                                                                  $applicative1
   `|*>` can be used to specify concurrent execution + `play`                                                $applicative2
   by using an applicative result type, like `Future` and the `|@` operator                                  $applicative3
   `|@>` can be used to specify concurrent execution + `play`                                                $applicative4
   A call to |* returns (es: ExecutorService) => Result                                                      $applicative5

 Even if the execution is concurrent you will get the errors corresponding to each row                       $applicative6
                                                                                                             """
  def boom = error("boom")

  def e1 =
    "a"   | "b" | "c" |>
     2    !  2  !  4  |
     1    !  1  !  2  | { (a, b, c) =>  a + b must_== c }

  def e2 = // if the table was executed, it would go "boom"
    "a"   | "b" | "c" |
     2    !  2  !  4  |
     1    !  1  !  2  | { (a, b, c) => boom; a + b must_== c }

  def e3 =
   ("a"   | "b" | "c" |
     2    !  2  !  4  |
     1    !  1  !  3  |> { (a, b, c) => a + b must_== c }) must beFailing

  def e4 =
   ("a"   | "b" | "c" |
     2    !  2  !  4  |
     1    !  1  !  3  |> { (a, b, c) => boom; a + b must_== c }) must beError

  def e5 =
    "a"     |  "b"      | "c"             |
    "Hello" !! "world"  !  "Hello world"  |> { (a, b, c) =>  a +" "+b must_== c }

  def e6 =
    "a"     ||  "b"      | "c"            |
    "Hello" !! "world"   !  "Hello world" |
    1       !! "world"   !  "1 world"     |> { (a, b, c) =>  a +" "+b must_== c }

  def e7 =
    "a"   |
     2    |
     1    |> { (a) =>  a must be_>=(0) }


  def e8 =
    "a"         | "b"       |>
    0           ! "0"       |
    List("a")   ! "List(a)" | { (a, b) =>  a.toString must_== b }

  def e9 = {
    "a successful table does not throw an exception" ==> {
      (new InAMutableContext).resultOk must not (throwAn[Exception])
    } and
    "a failed table throws an exception" ==> {
      (new InAMutableContext).resultKo must throwAn[Exception]
    }
  }

  def e10 = {
    val table =
      "a"     || "b"     | "c"         |>
      "hello" !! "you"   ! "hello you" |
      "you"   !! "hello" ! "you hello" |
      "y"     !! "h"     ! "y h"       | { (a, b, c) =>  a+" "+b must_== c }

    table.message ===
      "  | a     | b     | c         | "+"\n"+
      "+ | hello | you   | hello you | "+"\n"+
      "+ | you   | hello | you hello | "+"\n"+
      "+ | y     | h     | y h       | "
  }

  def e11 = {
    val table =
      "a"     || "b"     | "c"          |>
      "hello" !! "you"   ! "hello you"  |
      "you"   !! "hello" ! "you hello2" |
      "y"     !! "h"     ! "y h"        | { (a, b, c) =>  a+" "+b must_== c }

    table.message ===
      "  | a     | b     | c          |                                         "+"\n"+
      "+ | hello | you   | hello you  |                                         "+"\n"+
      "x | you   | hello | you hello2 | 'you hello' is not equal to 'you hello2'"+"\n"+
      "+ | y     | h     | y h        |                                         "
  }

  def e12 = {
    val t1 =
      "a"   | "b" | "c" |>
       2    !  2  !  4  |
       1    !  1  !  2  | { (a, b, c) =>  a + b must_== c }

    val t2 =
      "a"   | "b" | "c" |>
       2    !  2  !  5  | { (a, b, c) =>  a + b must_== c }

    (AsResult(t1) and t2).message ===
      "  | a | b | c |                        "+"\n"+
      "x | 2 | 2 | 5 | '4' is not equal to '5'"
  }

  def e13 =
    "a"            || "b"    |>
    "a"            !! "b"    |
    (null: String) !! ""     | { (a, b) =>  ok }

  def format1 = {
    val showValue = (i: Int) => "the value "+i
    val showResult = (s: String) => "the result "+s
    val table =
      "a"   | "b"   |>
       2    !  "2"  |
       1    !  "3"  | { (a, b) =>  a.toString must_== b } showAs (show1 = showValue, showResult)

    table.message ===
      "  | a           | b            |                        \n"+
      "+ | the value 2 | the result 2 |                        \n"+
      "x | the value 1 | the result 3 | '1' is not equal to '3'"
  }

  def format2 = {
    val spec = new InAMutableContext
    def table = spec.resultKo
    import spec.DataTableResult2._

    (table must throwAn[Exception]) and
    (ResultExecution.execute(AsResult(table)).message === "xxx")
  }.pendingUntilFixed

  def applicative1 = {
    "a" | "b" |>
    1   ! "1" |
    2   ! "2" |* { (a: Int, b: String) => a ==== b.toInt }
  }

  def applicative2 = {
    "a" | "b" |
    1   ! "1" |
    2   ! "2" |*> { (a: Int, b: String) => a ==== b.toInt }
  }


  def applicative3 = { implicit ee: ExecutionEnv =>
    "a" | "b" |>
    1   ! "1" |
    2   ! "2" |@ { (a: Int, b: String) => Future.delay(a ==== b.toInt) } attempt
  }

  def applicative4 = { implicit ee: ExecutionEnv =>
    "a" | "b" |
    1   ! "1" |
    2   ! "2" |@> { (a: Int, b: String) => Future.delay(a ==== b.toInt) } attempt
  }

  def applicative5 = {
    "a" | "b" |>
    1   ! "1" |
    2   ! "2" |* { (a: Int, b: String) => a ==== b.toInt }
  }

  def applicative6 = { implicit es: ExecutorService =>
    val table =
      "a" | "b" |>
      1   ! "1" |
      2   ! "0" |
      3   ! "3" |* { (a: Int, b: String) => a ==== b.toInt }

    table(es).message ===
      "  | a | b |                        "+"\n"+
      "+ | 1 | 1 |                        "+"\n"+
      "x | 2 | 0 | '2' is not equal to '0'"+"\n"+
      "+ | 3 | 3 |                        "

  }

  implicit def DataTableResult1ToResult[T1](d: DataTableResult1[T1]): Result =
    AsResult(d)

  implicit def DataTableResult2ToResult[T1, T2](d: DataTableResult2[T1, T2]): Result =
    AsResult(d)

  implicit def DataTableResult3ToResult[T1, T2, T3](d: DataTableResult3[T1, T2, T3]): Result =
    AsResult(d)

}

class InAMutableContext extends MustThrownMatchers with DataTables {
  lazy val resultOk =
      "a" | "b"    |>
       1  ! 1      | { (a, b) =>  a must_== b }

  lazy val resultKo =
    "a" | "b"    |>
     1  ! 2      | { (a, b) =>  a must_== b } //showAs(show1 = (i:Int) => (i+1).toString)
}

