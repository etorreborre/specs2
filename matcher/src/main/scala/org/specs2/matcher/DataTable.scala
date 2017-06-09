package org.specs2
package matcher

import execute._
import ResultExecution._
import text.{NotNullStrings, Show1, Show10, Show2, Show3, Show4, Show5, Show6, Show7, Show8, Show9, TextTable, Trim}
import Trim._
import NotNullStrings._
import ResultLogicalCombinators._

import scalaz.{Success => _, _}
import Scalaz._
import scala.concurrent._, duration._

/**
 * This trait provides implicit definitions and types to create DataTables.
 * 
 * A DataTable has a header defining column names and rows holding values.
 * It is possible to apply a function taking the row values and returning a MatchResult.
 * 
 * A TableHeader is defined by separating the column names with '|':
 * ` "a" | "b" | "c"`  
 * 
 * A DataRow is defined by separating the row values with '!':
 * ` 1 ! 2 ! 3`  
 * 
 * Note that the '!' method can conflict with the creation of Examples when the value is a 
 * string. In that case it is possible to use the '!!! method to disambiguate:
 * 
 * `"1" !! "2" ! "3"`
 * 
 * In that case the first column of the header can also be defined with '||' for pure
 * symmetry reasons:
 *  
 * `"a" || "b" | "c"`  
 * `"1" !! "2" ! "3"`
 * 
 * @see org.specs2.matcher.DataTablesSpec for examples
 */
trait DataTables extends ExpectationsCreation {
  
  /** @return a TableHeader with one heading only */
  implicit def toTableHeader(a: String) = new TableHeader(List(a))
  /** @return a DataRow with one value only */
  implicit def toDataRow[T](a: T) = DataRow1(a)

  /**
   * A DataTable with its header
   *
   * Children of this class are parametrized with the types of values that their rows can
   * hold.
   */
  abstract class Table(val titles: List[String], val execute: Boolean = false) {

    /**
     * Collect the results of each row
     * @param results list of (row description, row execution result)
     * @return an aggregated Result from a list of results
     */
    protected def collect[R : AsResult](results: List[(Seq[String], R)]): DecoratedResult[DataTable] = {
      val result = allSuccess(results)
      val decorated =
        DecoratedResult(DataTable(titles, results), result.updateMessage {
           TextTable("" +: titles :+ "", results.map { case (line, r) => resultLine(line, AsResult(r)) }:_*).show
        })
      checkResultFailure(decorated)
      decorated
    }
    /** @return the logical and combination of all the results */
    private def allSuccess[R : AsResult](results: List[(Seq[String], R)]): Result = {
      results.foldLeft(Success("", results.size): Result)((res, cur) => res and AsResult(cur._2))
    }
    /** @return the status of the row + the values + the failure message if any */
    private def resultLine(line: Seq[String], result: Result): Seq[String] = {
      val message = if (result.isSuccess) "" else result.message
      result.status +: line :+ message
    }
  }
  /** GENERATED */
  case class TableHeader(titles: List[String]) {
    def |(title: String) = copy(titles = this.titles :+ title)
    def ||(title: String) = copy(titles = this.titles :+ title)
    def |[T1](row: DataRow1[T1]) = new Table1(titles, List(row))
    def |>[T1](row: DataRow1[T1]) = new Table1(titles, List(row), execute = true)
    def |[T1, T2](row: DataRow2[T1, T2]) = new Table2(titles, List(row))
    def |>[T1, T2](row: DataRow2[T1, T2]) = new Table2(titles, List(row), execute = true)
    def |[T1, T2, T3](row: DataRow3[T1, T2, T3]) = new Table3(titles, List(row))
    def |>[T1, T2, T3](row: DataRow3[T1, T2, T3]) = new Table3(titles, List(row), execute = true)
    def |[T1, T2, T3, T4](row: DataRow4[T1, T2, T3, T4]) = new Table4(titles, List(row))
    def |>[T1, T2, T3, T4](row: DataRow4[T1, T2, T3, T4]) = new Table4(titles, List(row), execute = true)
    def |[T1, T2, T3, T4, T5](row: DataRow5[T1, T2, T3, T4, T5]) = new Table5(titles, List(row))
    def |>[T1, T2, T3, T4, T5](row: DataRow5[T1, T2, T3, T4, T5]) = new Table5(titles, List(row), execute = true)
    def |[T1, T2, T3, T4, T5, T6](row: DataRow6[T1, T2, T3, T4, T5, T6]) = new Table6(titles, List(row))
    def |>[T1, T2, T3, T4, T5, T6](row: DataRow6[T1, T2, T3, T4, T5, T6]) = new Table6(titles, List(row), execute = true)
    def |[T1, T2, T3, T4, T5, T6, T7](row: DataRow7[T1, T2, T3, T4, T5, T6, T7]) = new Table7(titles, List(row))
    def |>[T1, T2, T3, T4, T5, T6, T7](row: DataRow7[T1, T2, T3, T4, T5, T6, T7]) = new Table7(titles, List(row), execute = true)
    def |[T1, T2, T3, T4, T5, T6, T7, T8](row: DataRow8[T1, T2, T3, T4, T5, T6, T7, T8]) = new Table8(titles, List(row))
    def |>[T1, T2, T3, T4, T5, T6, T7, T8](row: DataRow8[T1, T2, T3, T4, T5, T6, T7, T8]) = new Table8(titles, List(row), execute = true)
    def |[T1, T2, T3, T4, T5, T6, T7, T8, T9](row: DataRow9[T1, T2, T3, T4, T5, T6, T7, T8, T9]) = new Table9(titles, List(row))
    def |>[T1, T2, T3, T4, T5, T6, T7, T8, T9](row: DataRow9[T1, T2, T3, T4, T5, T6, T7, T8, T9]) = new Table9(titles, List(row), execute = true)
    def |[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](row: DataRow10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]) = new Table10(titles, List(row))
    def |>[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](row: DataRow10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]) = new Table10(titles, List(row), execute = true)
  }

  case class Table1[T1](override val titles: List[String], rows: List[DataRow1[T1]], override val execute: Boolean = false, show1: Show1[T1] = Show1[T1]()) extends Table(titles, execute) { outer =>
    def |[S1 >: T1](row: DataRow1[S1])(implicit s1: Show1[S1] = Show1[S1]()) = Table1(titles, outer.rows :+ row, execute, s1)
    def |[R : AsResult](f: (T1) => R) = executeRow(f, execute)
    def |>[R : AsResult](f: (T1) => R) = executeRow(f, true)
    def executeRow[R : AsResult](f: (T1) => R, exec: Boolean): DecoratedResult[DataTable] = {
      if (exec)
        collect(rows map { (d: DataRow1[T1]) => (show1.showList(d.t1), AsResult(f(d.t1)).execute) })
      else DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok"))
    }
    def |@[M[_], R](f: (T1) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] =
      executeRowApply(f, execute)
    def |@>[M[_], R](f: (T1) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] =
      executeRowApply(f, true)
    def |*[R](f: (T1) => R)(implicit asResult: AsResult[R], ec: ExecutionContext): DecoratedResult[DataTable] =
      executeRowApply((t1: T1) => Future(f(t1))(ec), execute)(asResult, control.FutureInstances.parallelApplicative(ec)).run
    def |*>[R](f: (T1) => R)(implicit asResult: AsResult[R], ec: ExecutionContext): DecoratedResult[DataTable] =
      executeRowApply((t1: T1) => Future(f(t1))(ec), true)(asResult, control.FutureInstances.parallelApplicative(ec)).run
    def executeRowApply[R, M[_]](f: (T1) => M[R], exec: Boolean)(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] = {
      if (exec)
        app.map(app.traverse(rows) { d: DataRow1[T1] =>
          app.map(f(d.t1))(r => (show1.showList(d.t1), AsResult(r).execute))
        })(rs => collect(rs))
      else app.pure(DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok")))
    }
  }
  case class Table2[T1, T2](override val titles: List[String], rows: List[DataRow2[T1, T2]], override val execute: Boolean = false, show2: Show2[T1, T2] = Show2[T1, T2]()) extends Table(titles, execute) { outer =>
    def |[S1 >: T1, S2 >: T2](row: DataRow2[S1, S2])(implicit s2: Show2[S1, S2] = Show2[S1, S2]()) = Table2(titles, outer.rows :+ row, execute, s2)
    def |[R : AsResult](f: (T1, T2) => R) = executeRow(f, execute)
    def |>[R : AsResult](f: (T1, T2) => R) = executeRow(f, true)
    def executeRow[R : AsResult](f: (T1, T2) => R, exec: Boolean): DecoratedResult[DataTable] = {
      if (exec)
        collect(rows map { (d: DataRow2[T1, T2]) => (show2.showList(d.t1,d.t2), AsResult(f(d.t1,d.t2)).execute) })
      else DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok"))
    }
    def |@[M[_], R](f: (T1, T2) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] =
      executeRowApply(f, execute)
    def |@>[M[_], R](f: (T1, T2) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] =
      executeRowApply(f, true)
    def |*[R](f: (T1, T2) => R)(implicit asResult: AsResult[R], ec: ExecutionContext): DecoratedResult[DataTable] =
      executeRowApply((t1: T1, t2: T2) => Future(f(t1,t2))(ec), execute)(asResult, control.FutureInstances.parallelApplicative(ec)).run
    def |*>[R](f: (T1, T2) => R)(implicit asResult: AsResult[R], ec: ExecutionContext): DecoratedResult[DataTable] =
      executeRowApply((t1: T1, t2: T2) => Future(f(t1,t2))(ec), true)(asResult, control.FutureInstances.parallelApplicative(ec)).run
    def executeRowApply[R, M[_]](f: (T1, T2) => M[R], exec: Boolean)(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] = {
      if (exec)
        app.map(app.traverse(rows) { d: DataRow2[T1, T2] =>
          app.map(f(d.t1,d.t2))(r => (show2.showList(d.t1,d.t2), AsResult(r).execute))
        })(rs => collect(rs))
      else app.pure(DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok")))
    }
  }
  case class Table3[T1, T2, T3](override val titles: List[String], rows: List[DataRow3[T1, T2, T3]], override val execute: Boolean = false, show3: Show3[T1, T2, T3] = Show3[T1, T2, T3]()) extends Table(titles, execute) { outer =>
    def |[S1 >: T1, S2 >: T2, S3 >: T3](row: DataRow3[S1, S2, S3])(implicit s3: Show3[S1, S2, S3] = Show3[S1, S2, S3]()) = Table3(titles, outer.rows :+ row, execute, s3)
    def |[R : AsResult](f: (T1, T2, T3) => R) = executeRow(f, execute)
    def |>[R : AsResult](f: (T1, T2, T3) => R) = executeRow(f, true)
    def executeRow[R : AsResult](f: (T1, T2, T3) => R, exec: Boolean): DecoratedResult[DataTable] = {
      if (exec)
        collect(rows map { (d: DataRow3[T1, T2, T3]) => (show3.showList(d.t1,d.t2,d.t3), AsResult(f(d.t1,d.t2,d.t3)).execute) })
      else DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok"))
    }
    def |@[M[_], R](f: (T1, T2, T3) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] =
      executeRowApply(f, execute)
    def |@>[M[_], R](f: (T1, T2, T3) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] =
      executeRowApply(f, true)
    def |*[R](f: (T1, T2, T3) => R)(implicit asResult: AsResult[R], ec: ExecutionContext): DecoratedResult[DataTable] =
      executeRowApply((t1: T1, t2: T2, t3: T3) => Future(f(t1,t2,t3))(ec), execute)(asResult, control.FutureInstances.parallelApplicative(ec)).run
    def |*>[R](f: (T1, T2, T3) => R)(implicit asResult: AsResult[R], ec: ExecutionContext): DecoratedResult[DataTable] =
      executeRowApply((t1: T1, t2: T2, t3: T3) => Future(f(t1,t2,t3))(ec), true)(asResult, control.FutureInstances.parallelApplicative(ec)).run
    def executeRowApply[R, M[_]](f: (T1, T2, T3) => M[R], exec: Boolean)(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] = {
      if (exec)
        app.map(app.traverse(rows) { d: DataRow3[T1, T2, T3] =>
          app.map(f(d.t1,d.t2,d.t3))(r => (show3.showList(d.t1,d.t2,d.t3), AsResult(r).execute))
        })(rs => collect(rs))
      else app.pure(DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok")))
    }
  }
  case class Table4[T1, T2, T3, T4](override val titles: List[String], rows: List[DataRow4[T1, T2, T3, T4]], override val execute: Boolean = false, show4: Show4[T1, T2, T3, T4] = Show4[T1, T2, T3, T4]()) extends Table(titles, execute) { outer =>
    def |[S1 >: T1, S2 >: T2, S3 >: T3, S4 >: T4](row: DataRow4[S1, S2, S3, S4])(implicit s4: Show4[S1, S2, S3, S4] = Show4[S1, S2, S3, S4]()) = Table4(titles, outer.rows :+ row, execute, s4)
    def |[R : AsResult](f: (T1, T2, T3, T4) => R) = executeRow(f, execute)
    def |>[R : AsResult](f: (T1, T2, T3, T4) => R) = executeRow(f, true)
    def executeRow[R : AsResult](f: (T1, T2, T3, T4) => R, exec: Boolean): DecoratedResult[DataTable] = {
      if (exec)
        collect(rows map { (d: DataRow4[T1, T2, T3, T4]) => (show4.showList(d.t1,d.t2,d.t3,d.t4), AsResult(f(d.t1,d.t2,d.t3,d.t4)).execute) })
      else DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok"))
    }
    def |@[M[_], R](f: (T1, T2, T3, T4) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] =
      executeRowApply(f, execute)
    def |@>[M[_], R](f: (T1, T2, T3, T4) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] =
      executeRowApply(f, true)
    def |*[R](f: (T1, T2, T3, T4) => R)(implicit asResult: AsResult[R], ec: ExecutionContext): DecoratedResult[DataTable] =
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4) => Future(f(t1,t2,t3,t4))(ec), execute)(asResult, control.FutureInstances.parallelApplicative(ec)).run
    def |*>[R](f: (T1, T2, T3, T4) => R)(implicit asResult: AsResult[R], ec: ExecutionContext): DecoratedResult[DataTable] =
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4) => Future(f(t1,t2,t3,t4))(ec), true)(asResult, control.FutureInstances.parallelApplicative(ec)).run
    def executeRowApply[R, M[_]](f: (T1, T2, T3, T4) => M[R], exec: Boolean)(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] = {
      if (exec)
        app.map(app.traverse(rows) { d: DataRow4[T1, T2, T3, T4] =>
          app.map(f(d.t1,d.t2,d.t3,d.t4))(r => (show4.showList(d.t1,d.t2,d.t3,d.t4), AsResult(r).execute))
        })(rs => collect(rs))
      else app.pure(DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok")))
    }
  }
  case class Table5[T1, T2, T3, T4, T5](override val titles: List[String], rows: List[DataRow5[T1, T2, T3, T4, T5]], override val execute: Boolean = false, show5: Show5[T1, T2, T3, T4, T5] = Show5[T1, T2, T3, T4, T5]()) extends Table(titles, execute) { outer =>
    def |[S1 >: T1, S2 >: T2, S3 >: T3, S4 >: T4, S5 >: T5](row: DataRow5[S1, S2, S3, S4, S5])(implicit s5: Show5[S1, S2, S3, S4, S5] = Show5[S1, S2, S3, S4, S5]()) = Table5(titles, outer.rows :+ row, execute, s5)
    def |[R : AsResult](f: (T1, T2, T3, T4, T5) => R) = executeRow(f, execute)
    def |>[R : AsResult](f: (T1, T2, T3, T4, T5) => R) = executeRow(f, true)
    def executeRow[R : AsResult](f: (T1, T2, T3, T4, T5) => R, exec: Boolean): DecoratedResult[DataTable] = {
      if (exec)
        collect(rows map { (d: DataRow5[T1, T2, T3, T4, T5]) => (show5.showList(d.t1,d.t2,d.t3,d.t4,d.t5), AsResult(f(d.t1,d.t2,d.t3,d.t4,d.t5)).execute) })
      else DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok"))
    }
    def |@[M[_], R](f: (T1, T2, T3, T4, T5) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] =
      executeRowApply(f, execute)
    def |@>[M[_], R](f: (T1, T2, T3, T4, T5) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] =
      executeRowApply(f, true)
    def |*[R](f: (T1, T2, T3, T4, T5) => R)(implicit asResult: AsResult[R], ec: ExecutionContext): DecoratedResult[DataTable] =
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5) => Future(f(t1,t2,t3,t4,t5))(ec), execute)(asResult, control.FutureInstances.parallelApplicative(ec)).run
    def |*>[R](f: (T1, T2, T3, T4, T5) => R)(implicit asResult: AsResult[R], ec: ExecutionContext): DecoratedResult[DataTable] =
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5) => Future(f(t1,t2,t3,t4,t5))(ec), true)(asResult, control.FutureInstances.parallelApplicative(ec)).run
    def executeRowApply[R, M[_]](f: (T1, T2, T3, T4, T5) => M[R], exec: Boolean)(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] = {
      if (exec)
        app.map(app.traverse(rows) { d: DataRow5[T1, T2, T3, T4, T5] =>
          app.map(f(d.t1,d.t2,d.t3,d.t4,d.t5))(r => (show5.showList(d.t1,d.t2,d.t3,d.t4,d.t5), AsResult(r).execute))
        })(rs => collect(rs))
      else app.pure(DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok")))
    }
  }
  case class Table6[T1, T2, T3, T4, T5, T6](override val titles: List[String], rows: List[DataRow6[T1, T2, T3, T4, T5, T6]], override val execute: Boolean = false, show6: Show6[T1, T2, T3, T4, T5, T6] = Show6[T1, T2, T3, T4, T5, T6]()) extends Table(titles, execute) { outer =>
    def |[S1 >: T1, S2 >: T2, S3 >: T3, S4 >: T4, S5 >: T5, S6 >: T6](row: DataRow6[S1, S2, S3, S4, S5, S6])(implicit s6: Show6[S1, S2, S3, S4, S5, S6] = Show6[S1, S2, S3, S4, S5, S6]()) = Table6(titles, outer.rows :+ row, execute, s6)
    def |[R : AsResult](f: (T1, T2, T3, T4, T5, T6) => R) = executeRow(f, execute)
    def |>[R : AsResult](f: (T1, T2, T3, T4, T5, T6) => R) = executeRow(f, true)
    def executeRow[R : AsResult](f: (T1, T2, T3, T4, T5, T6) => R, exec: Boolean): DecoratedResult[DataTable] = {
      if (exec)
        collect(rows map { (d: DataRow6[T1, T2, T3, T4, T5, T6]) => (show6.showList(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6), AsResult(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6)).execute) })
      else DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok"))
    }
    def |@[M[_], R](f: (T1, T2, T3, T4, T5, T6) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] =
      executeRowApply(f, execute)
    def |@>[M[_], R](f: (T1, T2, T3, T4, T5, T6) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] =
      executeRowApply(f, true)
    def |*[R](f: (T1, T2, T3, T4, T5, T6) => R)(implicit asResult: AsResult[R], ec: ExecutionContext): DecoratedResult[DataTable] =
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6) => Future(f(t1,t2,t3,t4,t5,t6))(ec), execute)(asResult, control.FutureInstances.parallelApplicative(ec)).run
    def |*>[R](f: (T1, T2, T3, T4, T5, T6) => R)(implicit asResult: AsResult[R], ec: ExecutionContext): DecoratedResult[DataTable] =
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6) => Future(f(t1,t2,t3,t4,t5,t6))(ec), true)(asResult, control.FutureInstances.parallelApplicative(ec)).run
    def executeRowApply[R, M[_]](f: (T1, T2, T3, T4, T5, T6) => M[R], exec: Boolean)(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] = {
      if (exec)
        app.map(app.traverse(rows) { d: DataRow6[T1, T2, T3, T4, T5, T6] =>
          app.map(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6))(r => (show6.showList(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6), AsResult(r).execute))
        })(rs => collect(rs))
      else app.pure(DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok")))
    }
  }
  case class Table7[T1, T2, T3, T4, T5, T6, T7](override val titles: List[String], rows: List[DataRow7[T1, T2, T3, T4, T5, T6, T7]], override val execute: Boolean = false, show7: Show7[T1, T2, T3, T4, T5, T6, T7] = Show7[T1, T2, T3, T4, T5, T6, T7]()) extends Table(titles, execute) { outer =>
    def |[S1 >: T1, S2 >: T2, S3 >: T3, S4 >: T4, S5 >: T5, S6 >: T6, S7 >: T7](row: DataRow7[S1, S2, S3, S4, S5, S6, S7])(implicit s7: Show7[S1, S2, S3, S4, S5, S6, S7] = Show7[S1, S2, S3, S4, S5, S6, S7]()) = Table7(titles, outer.rows :+ row, execute, s7)
    def |[R : AsResult](f: (T1, T2, T3, T4, T5, T6, T7) => R) = executeRow(f, execute)
    def |>[R : AsResult](f: (T1, T2, T3, T4, T5, T6, T7) => R) = executeRow(f, true)
    def executeRow[R : AsResult](f: (T1, T2, T3, T4, T5, T6, T7) => R, exec: Boolean): DecoratedResult[DataTable] = {
      if (exec)
        collect(rows map { (d: DataRow7[T1, T2, T3, T4, T5, T6, T7]) => (show7.showList(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7), AsResult(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7)).execute) })
      else DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok"))
    }
    def |@[M[_], R](f: (T1, T2, T3, T4, T5, T6, T7) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] =
      executeRowApply(f, execute)
    def |@>[M[_], R](f: (T1, T2, T3, T4, T5, T6, T7) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] =
      executeRowApply(f, true)
    def |*[R](f: (T1, T2, T3, T4, T5, T6, T7) => R)(implicit asResult: AsResult[R], ec: ExecutionContext): DecoratedResult[DataTable] =
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7) => Future(f(t1,t2,t3,t4,t5,t6,t7))(ec), execute)(asResult, control.FutureInstances.parallelApplicative(ec)).run
    def |*>[R](f: (T1, T2, T3, T4, T5, T6, T7) => R)(implicit asResult: AsResult[R], ec: ExecutionContext): DecoratedResult[DataTable] =
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7) => Future(f(t1,t2,t3,t4,t5,t6,t7))(ec), true)(asResult, control.FutureInstances.parallelApplicative(ec)).run
    def executeRowApply[R, M[_]](f: (T1, T2, T3, T4, T5, T6, T7) => M[R], exec: Boolean)(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] = {
      if (exec)
        app.map(app.traverse(rows) { d: DataRow7[T1, T2, T3, T4, T5, T6, T7] =>
          app.map(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7))(r => (show7.showList(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7), AsResult(r).execute))
        })(rs => collect(rs))
      else app.pure(DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok")))
    }
  }
  case class Table8[T1, T2, T3, T4, T5, T6, T7, T8](override val titles: List[String], rows: List[DataRow8[T1, T2, T3, T4, T5, T6, T7, T8]], override val execute: Boolean = false, show8: Show8[T1, T2, T3, T4, T5, T6, T7, T8] = Show8[T1, T2, T3, T4, T5, T6, T7, T8]()) extends Table(titles, execute) { outer =>
    def |[S1 >: T1, S2 >: T2, S3 >: T3, S4 >: T4, S5 >: T5, S6 >: T6, S7 >: T7, S8 >: T8](row: DataRow8[S1, S2, S3, S4, S5, S6, S7, S8])(implicit s8: Show8[S1, S2, S3, S4, S5, S6, S7, S8] = Show8[S1, S2, S3, S4, S5, S6, S7, S8]()) = Table8(titles, outer.rows :+ row, execute, s8)
    def |[R : AsResult](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R) = executeRow(f, execute)
    def |>[R : AsResult](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R) = executeRow(f, true)
    def executeRow[R : AsResult](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R, exec: Boolean): DecoratedResult[DataTable] = {
      if (exec)
        collect(rows map { (d: DataRow8[T1, T2, T3, T4, T5, T6, T7, T8]) => (show8.showList(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7,d.t8), AsResult(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7,d.t8)).execute) })
      else DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok"))
    }
    def |@[M[_], R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] =
      executeRowApply(f, execute)
    def |@>[M[_], R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] =
      executeRowApply(f, true)
    def |*[R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(implicit asResult: AsResult[R], ec: ExecutionContext): DecoratedResult[DataTable] =
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8) => Future(f(t1,t2,t3,t4,t5,t6,t7,t8))(ec), execute)(asResult, control.FutureInstances.parallelApplicative(ec)).run
    def |*>[R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(implicit asResult: AsResult[R], ec: ExecutionContext): DecoratedResult[DataTable] =
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8) => Future(f(t1,t2,t3,t4,t5,t6,t7,t8))(ec), true)(asResult, control.FutureInstances.parallelApplicative(ec)).run
    def executeRowApply[R, M[_]](f: (T1, T2, T3, T4, T5, T6, T7, T8) => M[R], exec: Boolean)(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] = {
      if (exec)
        app.map(app.traverse(rows) { d: DataRow8[T1, T2, T3, T4, T5, T6, T7, T8] =>
          app.map(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7,d.t8))(r => (show8.showList(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7,d.t8), AsResult(r).execute))
        })(rs => collect(rs))
      else app.pure(DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok")))
    }
  }
  case class Table9[T1, T2, T3, T4, T5, T6, T7, T8, T9](override val titles: List[String], rows: List[DataRow9[T1, T2, T3, T4, T5, T6, T7, T8, T9]], override val execute: Boolean = false, show9: Show9[T1, T2, T3, T4, T5, T6, T7, T8, T9] = Show9[T1, T2, T3, T4, T5, T6, T7, T8, T9]()) extends Table(titles, execute) { outer =>
    def |[S1 >: T1, S2 >: T2, S3 >: T3, S4 >: T4, S5 >: T5, S6 >: T6, S7 >: T7, S8 >: T8, S9 >: T9](row: DataRow9[S1, S2, S3, S4, S5, S6, S7, S8, S9])(implicit s9: Show9[S1, S2, S3, S4, S5, S6, S7, S8, S9] = Show9[S1, S2, S3, S4, S5, S6, S7, S8, S9]()) = Table9(titles, outer.rows :+ row, execute, s9)
    def |[R : AsResult](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R) = executeRow(f, execute)
    def |>[R : AsResult](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R) = executeRow(f, true)
    def executeRow[R : AsResult](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R, exec: Boolean): DecoratedResult[DataTable] = {
      if (exec)
        collect(rows map { (d: DataRow9[T1, T2, T3, T4, T5, T6, T7, T8, T9]) => (show9.showList(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7,d.t8,d.t9), AsResult(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7,d.t8,d.t9)).execute) })
      else DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok"))
    }
    def |@[M[_], R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] =
      executeRowApply(f, execute)
    def |@>[M[_], R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] =
      executeRowApply(f, true)
    def |*[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R)(implicit asResult: AsResult[R], ec: ExecutionContext): DecoratedResult[DataTable] =
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9) => Future(f(t1,t2,t3,t4,t5,t6,t7,t8,t9))(ec), execute)(asResult, control.FutureInstances.parallelApplicative(ec)).run
    def |*>[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R)(implicit asResult: AsResult[R], ec: ExecutionContext): DecoratedResult[DataTable] =
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9) => Future(f(t1,t2,t3,t4,t5,t6,t7,t8,t9))(ec), true)(asResult, control.FutureInstances.parallelApplicative(ec)).run
    def executeRowApply[R, M[_]](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => M[R], exec: Boolean)(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] = {
      if (exec)
        app.map(app.traverse(rows) { d: DataRow9[T1, T2, T3, T4, T5, T6, T7, T8, T9] =>
          app.map(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7,d.t8,d.t9))(r => (show9.showList(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7,d.t8,d.t9), AsResult(r).execute))
        })(rs => collect(rs))
      else app.pure(DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok")))
    }
  }
  case class Table10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](override val titles: List[String], rows: List[DataRow10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]], override val execute: Boolean = false, show10: Show10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = Show10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]()) extends Table(titles, execute) { outer =>
    def |[S1 >: T1, S2 >: T2, S3 >: T3, S4 >: T4, S5 >: T5, S6 >: T6, S7 >: T7, S8 >: T8, S9 >: T9, S10 >: T10](row: DataRow10[S1, S2, S3, S4, S5, S6, S7, S8, S9, S10])(implicit s10: Show10[S1, S2, S3, S4, S5, S6, S7, S8, S9, S10] = Show10[S1, S2, S3, S4, S5, S6, S7, S8, S9, S10]()) = Table10(titles, outer.rows :+ row, execute, s10)
    def |[R : AsResult](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R) = executeRow(f, execute)
    def |>[R : AsResult](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R) = executeRow(f, true)
    def executeRow[R : AsResult](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R, exec: Boolean): DecoratedResult[DataTable] = {
      if (exec)
        collect(rows map { (d: DataRow10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]) => (show10.showList(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7,d.t8,d.t9,d.t10), AsResult(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7,d.t8,d.t9,d.t10)).execute) })
      else DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok"))
    }
    def |@[M[_], R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] =
      executeRowApply(f, execute)
    def |@>[M[_], R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] =
      executeRowApply(f, true)
    def |*[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R)(implicit asResult: AsResult[R], ec: ExecutionContext): DecoratedResult[DataTable] =
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10) => Future(f(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10))(ec), execute)(asResult, control.FutureInstances.parallelApplicative(ec)).run
    def |*>[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R)(implicit asResult: AsResult[R], ec: ExecutionContext): DecoratedResult[DataTable] =
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10) => Future(f(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10))(ec), true)(asResult, control.FutureInstances.parallelApplicative(ec)).run
    def executeRowApply[R, M[_]](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => M[R], exec: Boolean)(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] = {
      if (exec)
        app.map(app.traverse(rows) { d: DataRow10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] =>
          app.map(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7,d.t8,d.t9,d.t10))(r => (show10.showList(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7,d.t8,d.t9,d.t10), AsResult(r).execute))
        })(rs => collect(rs))
      else app.pure(DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok")))
    }
  }

  implicit class FutureOps[A](f: Future[A]) {
    def run: A =
      Await.result(f, Duration.Inf)
  }

  abstract class DataRow[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10] extends Product {
    def show = productIterator.mkString("|", "|", "|")
    def showCells = productIterator.map(_.notNull).toSeq
  }

  case class DataRow1[+T1](t1: T1) extends DataRow[T1, Any, Any, Any, Any, Any, Any, Any, Any, Any] {
    def ![S2](t2: S2) = DataRow2(t1, t2)
    def !![S2](t2: S2) = DataRow2(t1, t2)
  }
  case class DataRow2[+T1, +T2](t1: T1, t2: T2) extends DataRow[T1, T2, Any, Any, Any, Any, Any, Any, Any, Any] {
    def ![S3](t3: S3) = DataRow3(t1, t2, t3)
    def !![S3](t3: S3) = DataRow3(t1, t2, t3)
  }
  case class DataRow3[+T1, +T2, +T3](t1: T1, t2: T2, t3: T3) extends DataRow[T1, T2, T3, Any, Any, Any, Any, Any, Any, Any] {
    def ![S4](t4: S4) = DataRow4(t1, t2, t3, t4)
    def !![S4](t4: S4) = DataRow4(t1, t2, t3, t4)
  }
  case class DataRow4[+T1, +T2, +T3, +T4](t1: T1, t2: T2, t3: T3, t4: T4) extends DataRow[T1, T2, T3, T4, Any, Any, Any, Any, Any, Any] {
    def ![S5](t5: S5) = DataRow5(t1, t2, t3, t4, t5)
    def !![S5](t5: S5) = DataRow5(t1, t2, t3, t4, t5)
  }
  case class DataRow5[+T1, +T2, +T3, +T4, +T5](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5) extends DataRow[T1, T2, T3, T4, T5, Any, Any, Any, Any, Any] {
    def ![S6](t6: S6) = DataRow6(t1, t2, t3, t4, t5, t6)
    def !![S6](t6: S6) = DataRow6(t1, t2, t3, t4, t5, t6)
  }
  case class DataRow6[+T1, +T2, +T3, +T4, +T5, +T6](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6) extends DataRow[T1, T2, T3, T4, T5, T6, Any, Any, Any, Any] {
    def ![S7](t7: S7) = DataRow7(t1, t2, t3, t4, t5, t6, t7)
    def !![S7](t7: S7) = DataRow7(t1, t2, t3, t4, t5, t6, t7)
  }
  case class DataRow7[+T1, +T2, +T3, +T4, +T5, +T6, +T7](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7) extends DataRow[T1, T2, T3, T4, T5, T6, T7, Any, Any, Any] {
    def ![S8](t8: S8) = DataRow8(t1, t2, t3, t4, t5, t6, t7, t8)
    def !![S8](t8: S8) = DataRow8(t1, t2, t3, t4, t5, t6, t7, t8)
  }
  case class DataRow8[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8) extends DataRow[T1, T2, T3, T4, T5, T6, T7, T8, Any, Any] {
    def ![S9](t9: S9) = DataRow9(t1, t2, t3, t4, t5, t6, t7, t8, t9)
    def !![S9](t9: S9) = DataRow9(t1, t2, t3, t4, t5, t6, t7, t8, t9)
  }
  case class DataRow9[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9) extends DataRow[T1, T2, T3, T4, T5, T6, T7, T8, T9, Any] {
    def ![S10](t10: S10) = DataRow10(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)
    def !![S10](t10: S10) = DataRow10(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)
  }
  case class DataRow10[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10) extends DataRow[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] {


  }

}

case class DataTable(titles: Seq[String], rows: Seq[DataTableRow]) {
  def isSuccess = rows.forall(_.isSuccess)
  def show = TextTable(titles, rows.map(row => row.cells.map(_.toString))).show
}
object DataTable {
  def apply[R : AsResult](titles: Seq[String], results: Seq[(Seq[String], R)]): DataTable = DataTable(titles, results.collect { case (v, r) => DataTableRow(v, AsResult(r)) })
}
case class DataTableRow(cells: Seq[String], result: Result) {
  def isSuccess = result.isSuccess
}
object DataTableRow {
  def apply[R : AsResult](values: String, result: R): DataTableRow = DataTableRow(values.trimEnclosing("|").splitTrim("\\|"), AsResult(result))
}

private[specs2]
object DataTables extends DataTables

object DataTablesGenerator {
  def main(args: Array[String]) = {
    println(all(10))
  }
  def all(n: Int) = {
    List(tableHeader(n),
      tableClasses(n),
      dataRowClass(n),
      dataRowClasses(n)).mkString("\n\n").replace("\n", "\n  ")
  }

  def tableHeader(n: Int) = {
    "case class TableHeader(titles: List[String]) {\n"+
      "  def |(title: String) = copy(titles = this.titles :+ title)\n"+
      "  def ||(title: String) = copy(titles = this.titles :+ title)\n"+
       (1 to n).flatMap { i =>
        val addRow = types(i)+"(row: "+dataRow(i)+") = new "+table(i)+"(titles, List(row)"
        val addRowStill = "def |"+addRow
        val addRowExecute = "def |>"+addRow
        List(addRowStill, addRowExecute+", execute = true").map(_+")")
      }.mkString("  ", "\n  ", "\n") +
      "}"
  }

  def tableClasses(n: Int) = {
    (1 to n).map { i =>
      List("case class Table"+i+types(i)+"(override val titles: List[String], rows: List["+dataRow(i)+"], override val execute: Boolean = false, "+s"show$i: Show$i${types(i)} = Show$i${types(i)}()"+") extends "+
        "Table(titles, execute) { outer =>",
        "  def |"+st(i)+"(row: "+dataRow(i, letter="S")+")(implicit s"+i+": Show"+i+types(i, letter = "S")+" = Show"+i+types(i, letter = "S")+"()) = "+table(i)+"(titles, outer.rows :+ row, execute, s"+i+")",
        "  def |[R : AsResult](f: "+typesTuple(i)+" => R) = executeRow(f, execute)",
        "  def |>[R : AsResult](f: "+typesTuple(i)+" => R) = executeRow(f, true)",
        "  def executeRow[R : AsResult](f: "+typesTuple(i)+" => R, exec: Boolean): DecoratedResult[DataTable] = {",
        "    if (exec)",
        "      collect(rows map { (d: "+dataRow(i)+") => (show"+i+".showList("+(1 to i).map("d.t"+_).mkString(",")+"), AsResult(f("+(1 to i).map("d.t"+_).mkString(",")+")).execute) })",
        "    else DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success(\"ok\"))",
        "  }",
        "  def |@[M[_], R](f: "+typesTuple(i)+" => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] =",
        "    executeRowApply(f, execute)",
        "  def |@>[M[_], R](f: "+typesTuple(i)+" => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] =",
        "    executeRowApply(f, true)",
        "  def |*[R](f: "+typesTuple(i)+" => R)(implicit asResult: AsResult[R], ec: ExecutionContext): DecoratedResult[DataTable] = ",
        "    executeRowApply("+parametersList(i: Int)+" => Future(f("+(1 to i).map("t"+_).mkString(",")+"))(ec), execute)(asResult, control.FutureInstances.parallelApplicative(ec)).run",
        "  def |*>[R](f: "+typesTuple(i)+" => R)(implicit asResult: AsResult[R], ec: ExecutionContext): DecoratedResult[DataTable] = ",
        "    executeRowApply("+parametersList(i: Int)+" => Future(f("+(1 to i).map("t"+_).mkString(",")+"))(ec), true)(asResult, control.FutureInstances.parallelApplicative(ec)).run",
        "  def executeRowApply[R, M[_]](f: "+typesTuple(i)+" => M[R], exec: Boolean)(implicit asResult: AsResult[R], app: Applicative[M]): M[DecoratedResult[DataTable]] = {",
        "    if (exec)",
        "      app.map(app.traverse(rows) { d: DataRow"+i+types(i)+" =>",
        "        app.map(f("+(1 to i).map("d.t"+_).mkString(",")+"))(r => (show"+i+".showList("+(1 to i).map("d.t"+_).mkString(",")+"), AsResult(r).execute))",
        "      })(rs => collect(rs))",
        "    else app.pure(DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success(\"ok\")))",
        "  }",
        "}").mkString("\n")
    }.mkString("\n")
  }
  def dataRowClass(n: Int) = {
    "abstract class DataRow"+variantTypes(n)+" extends Product {\n"+
      "  def show = productIterator.mkString(\"|\", \"|\", \"|\")\n"+
      "  def showCells = productIterator.map(_.notNull).toSeq\n"+
    "}"
  }
  def dataRowClasses(n: Int) = {
    (1 to n).map { i =>
      List(
        "case class "+dataRowDecl(i)+parametersList(i)+" extends DataRow["+typesList(i, n)+"] {",
        if (i < n) "  def ![S"+(i+1)+"](t"+(i+1)+": S"+(i+1)+") = "+"DataRow"+(i+1)+parameters(i+1) else "",
        if (i < n) "  def !![S"+(i+1)+"](t"+(i+1)+": S"+(i+1)+") = "+"DataRow"+(i+1)+parameters(i+1) else "",
        "}").mkString("\n")
    }.mkString("\n")
  }
  def parametersList(i: Int) = (1 to i).map(j => "t"+j+": T"+j).mkString("(",", ", ")")
  def parameters(i: Int) = (1 to i).map("t"+_).mkString("(",", ", ")")
  def variantTypes(i: Int, letter: String = "T") = typesAsList(i, letter).map("+"+_).mkString("[",", ", "]")
  def st(i: Int) = (1 to i).map(j => "S"+j+" >: T"+j).mkString("[",", ", "]")
  def types(i: Int, letter: String = "T") = typesAsList(i, letter).mkString("[",", ", "]")
  def typesAsList(i: Int, letter: String = "T"): Seq[String] =  (1 to i).map(letter+_)
  def typesList(i: Int): String = typesAsList(i).mkString(", ")
  def typesList(i: Int, n: Int): String =  (List(typesList(i)) ::: (i+1 to n).map(t => "Any").toList).mkString(", ")
  def typesTuple(i: Int) =  typesAsList(i).mkString("(",", ", ")")
  def dataRowDecl(i: Int, letter: String = "T") = "DataRow"+i+variantTypes(i, letter)
  def dataRow(i: Int, letter: String = "T") = "DataRow"+i+typesAsList(i, letter).mkString("[",", ", "]")
  def table(i: Int) = "Table"+i
}

