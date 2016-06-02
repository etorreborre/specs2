package org.specs2
package matcher

import execute._
import ResultExecution._
import text.{Trim, TextTable, NotNullStrings}
import Trim._
import NotNullStrings._
import ResultLogicalCombinators._

import scalaz.{Success =>_,_}, Scalaz._
import scalaz.concurrent.Future
import java.util.concurrent.ExecutorService

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

  case class Table1[T1](override val titles: List[String], rows: List[DataRow1[T1]], override val execute: Boolean = false) extends Table(titles, execute) { outer =>
    def |[S1 >: T1](row: DataRow1[S1]) = Table1(titles, outer.rows :+ row, execute)
    def |[R : AsResult](f: (T1) => R) = executeRow(f, execute)
    def |>[R : AsResult](f: (T1) => R) = executeRow(f, true)
    def executeRow[R : AsResult](f: (T1) => R, exec: Boolean): DataTableResult1[T1] = {
      if (exec) DataTableResult1[T1](titles, rows map { d => (d, AsResult(f(d.t1)).execute) }).check
      else      DataTableResult1[T1](titles, List[(DataRow1[T1], Result)]())
    }
    def |@[M[_], R](f: (T1) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult1[T1]] =
      executeRowApply(f, execute)
    def |@>[M[_], R](f: (T1) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult1[T1]] =
      executeRowApply(f, true)
    def |*[R](f: (T1) => R)(implicit asResult: AsResult[R]): ExecutorService => DataTableResult1[T1] = (es: ExecutorService) =>
      executeRowApply((t1: T1) => Future.fork(Future.delay(f(t1)))(es), execute)(asResult, control.FutureInstances.parallelApplicative).run
    def |*>[R](f: (T1) => R)(implicit asResult: AsResult[R]): ExecutorService => DataTableResult1[T1] = (es: ExecutorService) =>
      executeRowApply((t1: T1) => Future.fork(Future.delay(f(t1)))(es), true)(asResult, control.FutureInstances.parallelApplicative).run
    def executeRowApply[R, M[_]](f: (T1) => M[R], exec: Boolean)(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult1[T1]] = {
      if (exec)
        app.map(app.traverse(rows.toList) { d: DataRow1[T1] =>
          app.map(f(d.t1))(r => (d, AsResult(r).execute))
        })(rs => DataTableResult1[T1](titles, rs))
      else app.pure(DataTableResult1[T1](titles, List[(DataRow1[T1], Result)]()))
    }
  }
  case class Table2[T1, T2](override val titles: List[String], rows: List[DataRow2[T1, T2]], override val execute: Boolean = false) extends Table(titles, execute) { outer =>
    def |[S1 >: T1, S2 >: T2](row: DataRow2[S1, S2]) = Table2(titles, outer.rows :+ row, execute)
    def |[R : AsResult](f: (T1, T2) => R) = executeRow(f, execute)
    def |>[R : AsResult](f: (T1, T2) => R) = executeRow(f, true)
    def executeRow[R : AsResult](f: (T1, T2) => R, exec: Boolean): DataTableResult2[T1, T2] = {
      if (exec) DataTableResult2[T1, T2](titles, rows map { d => (d, AsResult(f(d.t1,d.t2)).execute) }).check
      else      DataTableResult2[T1, T2](titles, List[(DataRow2[T1, T2], Result)]())
    }
    def |@[M[_], R](f: (T1, T2) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult2[T1, T2]] =
      executeRowApply(f, execute)
    def |@>[M[_], R](f: (T1, T2) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult2[T1, T2]] =
      executeRowApply(f, true)
    def |*[R](f: (T1, T2) => R)(implicit asResult: AsResult[R]): ExecutorService => DataTableResult2[T1, T2] = (es: ExecutorService) =>
      executeRowApply((t1: T1, t2: T2) => Future.fork(Future.delay(f(t1,t2)))(es), execute)(asResult, control.FutureInstances.parallelApplicative).run
    def |*>[R](f: (T1, T2) => R)(implicit asResult: AsResult[R]): ExecutorService => DataTableResult2[T1, T2] = (es: ExecutorService) =>
      executeRowApply((t1: T1, t2: T2) => Future.fork(Future.delay(f(t1,t2)))(es), true)(asResult, control.FutureInstances.parallelApplicative).run
    def executeRowApply[R, M[_]](f: (T1, T2) => M[R], exec: Boolean)(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult2[T1, T2]] = {
      if (exec)
        app.map(app.traverse(rows.toList) { d: DataRow2[T1, T2] =>
          app.map(f(d.t1,d.t2))(r => (d, AsResult(r).execute))
        })(rs => DataTableResult2[T1, T2](titles, rs))
      else app.pure(DataTableResult2[T1, T2](titles, List[(DataRow2[T1, T2], Result)]()))
    }
  }
  case class Table3[T1, T2, T3](override val titles: List[String], rows: List[DataRow3[T1, T2, T3]], override val execute: Boolean = false) extends Table(titles, execute) { outer =>
    def |[S1 >: T1, S2 >: T2, S3 >: T3](row: DataRow3[S1, S2, S3]) = Table3(titles, outer.rows :+ row, execute)
    def |[R : AsResult](f: (T1, T2, T3) => R) = executeRow(f, execute)
    def |>[R : AsResult](f: (T1, T2, T3) => R) = executeRow(f, true)
    def executeRow[R : AsResult](f: (T1, T2, T3) => R, exec: Boolean): DataTableResult3[T1, T2, T3] = {
      if (exec) DataTableResult3[T1, T2, T3](titles, rows map { d => (d, AsResult(f(d.t1,d.t2,d.t3)).execute) }).check
      else      DataTableResult3[T1, T2, T3](titles, List[(DataRow3[T1, T2, T3], Result)]())
    }
    def |@[M[_], R](f: (T1, T2, T3) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult3[T1, T2, T3]] =
      executeRowApply(f, execute)
    def |@>[M[_], R](f: (T1, T2, T3) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult3[T1, T2, T3]] =
      executeRowApply(f, true)
    def |*[R](f: (T1, T2, T3) => R)(implicit asResult: AsResult[R]): ExecutorService => DataTableResult3[T1, T2, T3] = (es: ExecutorService) =>
      executeRowApply((t1: T1, t2: T2, t3: T3) => Future.fork(Future.delay(f(t1,t2,t3)))(es), execute)(asResult, control.FutureInstances.parallelApplicative).run
    def |*>[R](f: (T1, T2, T3) => R)(implicit asResult: AsResult[R]): ExecutorService => DataTableResult3[T1, T2, T3] = (es: ExecutorService) =>
      executeRowApply((t1: T1, t2: T2, t3: T3) => Future.fork(Future.delay(f(t1,t2,t3)))(es), true)(asResult, control.FutureInstances.parallelApplicative).run
    def executeRowApply[R, M[_]](f: (T1, T2, T3) => M[R], exec: Boolean)(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult3[T1, T2, T3]] = {
      if (exec)
        app.map(app.traverse(rows.toList) { d: DataRow3[T1, T2, T3] =>
          app.map(f(d.t1,d.t2,d.t3))(r => (d, AsResult(r).execute))
        })(rs => DataTableResult3[T1, T2, T3](titles, rs))
      else app.pure(DataTableResult3[T1, T2, T3](titles, List[(DataRow3[T1, T2, T3], Result)]()))
    }
  }
  case class Table4[T1, T2, T3, T4](override val titles: List[String], rows: List[DataRow4[T1, T2, T3, T4]], override val execute: Boolean = false) extends Table(titles, execute) { outer =>
    def |[S1 >: T1, S2 >: T2, S3 >: T3, S4 >: T4](row: DataRow4[S1, S2, S3, S4]) = Table4(titles, outer.rows :+ row, execute)
    def |[R : AsResult](f: (T1, T2, T3, T4) => R) = executeRow(f, execute)
    def |>[R : AsResult](f: (T1, T2, T3, T4) => R) = executeRow(f, true)
    def executeRow[R : AsResult](f: (T1, T2, T3, T4) => R, exec: Boolean): DataTableResult4[T1, T2, T3, T4] = {
      if (exec) DataTableResult4[T1, T2, T3, T4](titles, rows map { d => (d, AsResult(f(d.t1,d.t2,d.t3,d.t4)).execute) }).check
      else      DataTableResult4[T1, T2, T3, T4](titles, List[(DataRow4[T1, T2, T3, T4], Result)]())
    }
    def |@[M[_], R](f: (T1, T2, T3, T4) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult4[T1, T2, T3, T4]] =
      executeRowApply(f, execute)
    def |@>[M[_], R](f: (T1, T2, T3, T4) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult4[T1, T2, T3, T4]] =
      executeRowApply(f, true)
    def |*[R](f: (T1, T2, T3, T4) => R)(implicit asResult: AsResult[R]): ExecutorService => DataTableResult4[T1, T2, T3, T4] = (es: ExecutorService) =>
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4) => Future.fork(Future.delay(f(t1,t2,t3,t4)))(es), execute)(asResult, control.FutureInstances.parallelApplicative).run
    def |*>[R](f: (T1, T2, T3, T4) => R)(implicit asResult: AsResult[R]): ExecutorService => DataTableResult4[T1, T2, T3, T4] = (es: ExecutorService) =>
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4) => Future.fork(Future.delay(f(t1,t2,t3,t4)))(es), true)(asResult, control.FutureInstances.parallelApplicative).run
    def executeRowApply[R, M[_]](f: (T1, T2, T3, T4) => M[R], exec: Boolean)(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult4[T1, T2, T3, T4]] = {
      if (exec)
        app.map(app.traverse(rows.toList) { d: DataRow4[T1, T2, T3, T4] =>
          app.map(f(d.t1,d.t2,d.t3,d.t4))(r => (d, AsResult(r).execute))
        })(rs => DataTableResult4[T1, T2, T3, T4](titles, rs))
      else app.pure(DataTableResult4[T1, T2, T3, T4](titles, List[(DataRow4[T1, T2, T3, T4], Result)]()))
    }
  }
  case class Table5[T1, T2, T3, T4, T5](override val titles: List[String], rows: List[DataRow5[T1, T2, T3, T4, T5]], override val execute: Boolean = false) extends Table(titles, execute) { outer =>
    def |[S1 >: T1, S2 >: T2, S3 >: T3, S4 >: T4, S5 >: T5](row: DataRow5[S1, S2, S3, S4, S5]) = Table5(titles, outer.rows :+ row, execute)
    def |[R : AsResult](f: (T1, T2, T3, T4, T5) => R) = executeRow(f, execute)
    def |>[R : AsResult](f: (T1, T2, T3, T4, T5) => R) = executeRow(f, true)
    def executeRow[R : AsResult](f: (T1, T2, T3, T4, T5) => R, exec: Boolean): DataTableResult5[T1, T2, T3, T4, T5] = {
      if (exec) DataTableResult5[T1, T2, T3, T4, T5](titles, rows map { d => (d, AsResult(f(d.t1,d.t2,d.t3,d.t4,d.t5)).execute) }).check
      else      DataTableResult5[T1, T2, T3, T4, T5](titles, List[(DataRow5[T1, T2, T3, T4, T5], Result)]())
    }
    def |@[M[_], R](f: (T1, T2, T3, T4, T5) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult5[T1, T2, T3, T4, T5]] =
      executeRowApply(f, execute)
    def |@>[M[_], R](f: (T1, T2, T3, T4, T5) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult5[T1, T2, T3, T4, T5]] =
      executeRowApply(f, true)
    def |*[R](f: (T1, T2, T3, T4, T5) => R)(implicit asResult: AsResult[R]): ExecutorService => DataTableResult5[T1, T2, T3, T4, T5] = (es: ExecutorService) =>
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5) => Future.fork(Future.delay(f(t1,t2,t3,t4,t5)))(es), execute)(asResult, control.FutureInstances.parallelApplicative).run
    def |*>[R](f: (T1, T2, T3, T4, T5) => R)(implicit asResult: AsResult[R]): ExecutorService => DataTableResult5[T1, T2, T3, T4, T5] = (es: ExecutorService) =>
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5) => Future.fork(Future.delay(f(t1,t2,t3,t4,t5)))(es), true)(asResult, control.FutureInstances.parallelApplicative).run
    def executeRowApply[R, M[_]](f: (T1, T2, T3, T4, T5) => M[R], exec: Boolean)(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult5[T1, T2, T3, T4, T5]] = {
      if (exec)
        app.map(app.traverse(rows.toList) { d: DataRow5[T1, T2, T3, T4, T5] =>
          app.map(f(d.t1,d.t2,d.t3,d.t4,d.t5))(r => (d, AsResult(r).execute))
        })(rs => DataTableResult5[T1, T2, T3, T4, T5](titles, rs))
      else app.pure(DataTableResult5[T1, T2, T3, T4, T5](titles, List[(DataRow5[T1, T2, T3, T4, T5], Result)]()))
    }
  }
  case class Table6[T1, T2, T3, T4, T5, T6](override val titles: List[String], rows: List[DataRow6[T1, T2, T3, T4, T5, T6]], override val execute: Boolean = false) extends Table(titles, execute) { outer =>
    def |[S1 >: T1, S2 >: T2, S3 >: T3, S4 >: T4, S5 >: T5, S6 >: T6](row: DataRow6[S1, S2, S3, S4, S5, S6]) = Table6(titles, outer.rows :+ row, execute)
    def |[R : AsResult](f: (T1, T2, T3, T4, T5, T6) => R) = executeRow(f, execute)
    def |>[R : AsResult](f: (T1, T2, T3, T4, T5, T6) => R) = executeRow(f, true)
    def executeRow[R : AsResult](f: (T1, T2, T3, T4, T5, T6) => R, exec: Boolean): DataTableResult6[T1, T2, T3, T4, T5, T6] = {
      if (exec) DataTableResult6[T1, T2, T3, T4, T5, T6](titles, rows map { d => (d, AsResult(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6)).execute) }).check
      else      DataTableResult6[T1, T2, T3, T4, T5, T6](titles, List[(DataRow6[T1, T2, T3, T4, T5, T6], Result)]())
    }
    def |@[M[_], R](f: (T1, T2, T3, T4, T5, T6) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult6[T1, T2, T3, T4, T5, T6]] =
      executeRowApply(f, execute)
    def |@>[M[_], R](f: (T1, T2, T3, T4, T5, T6) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult6[T1, T2, T3, T4, T5, T6]] =
      executeRowApply(f, true)
    def |*[R](f: (T1, T2, T3, T4, T5, T6) => R)(implicit asResult: AsResult[R]): ExecutorService => DataTableResult6[T1, T2, T3, T4, T5, T6] = (es: ExecutorService) =>
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6) => Future.fork(Future.delay(f(t1,t2,t3,t4,t5,t6)))(es), execute)(asResult, control.FutureInstances.parallelApplicative).run
    def |*>[R](f: (T1, T2, T3, T4, T5, T6) => R)(implicit asResult: AsResult[R]): ExecutorService => DataTableResult6[T1, T2, T3, T4, T5, T6] = (es: ExecutorService) =>
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6) => Future.fork(Future.delay(f(t1,t2,t3,t4,t5,t6)))(es), true)(asResult, control.FutureInstances.parallelApplicative).run
    def executeRowApply[R, M[_]](f: (T1, T2, T3, T4, T5, T6) => M[R], exec: Boolean)(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult6[T1, T2, T3, T4, T5, T6]] = {
      if (exec)
        app.map(app.traverse(rows.toList) { d: DataRow6[T1, T2, T3, T4, T5, T6] =>
          app.map(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6))(r => (d, AsResult(r).execute))
        })(rs => DataTableResult6[T1, T2, T3, T4, T5, T6](titles, rs))
      else app.pure(DataTableResult6[T1, T2, T3, T4, T5, T6](titles, List[(DataRow6[T1, T2, T3, T4, T5, T6], Result)]()))
    }
  }
  case class Table7[T1, T2, T3, T4, T5, T6, T7](override val titles: List[String], rows: List[DataRow7[T1, T2, T3, T4, T5, T6, T7]], override val execute: Boolean = false) extends Table(titles, execute) { outer =>
    def |[S1 >: T1, S2 >: T2, S3 >: T3, S4 >: T4, S5 >: T5, S6 >: T6, S7 >: T7](row: DataRow7[S1, S2, S3, S4, S5, S6, S7]) = Table7(titles, outer.rows :+ row, execute)
    def |[R : AsResult](f: (T1, T2, T3, T4, T5, T6, T7) => R) = executeRow(f, execute)
    def |>[R : AsResult](f: (T1, T2, T3, T4, T5, T6, T7) => R) = executeRow(f, true)
    def executeRow[R : AsResult](f: (T1, T2, T3, T4, T5, T6, T7) => R, exec: Boolean): DataTableResult7[T1, T2, T3, T4, T5, T6, T7] = {
      if (exec) DataTableResult7[T1, T2, T3, T4, T5, T6, T7](titles, rows map { d => (d, AsResult(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7)).execute) }).check
      else      DataTableResult7[T1, T2, T3, T4, T5, T6, T7](titles, List[(DataRow7[T1, T2, T3, T4, T5, T6, T7], Result)]())
    }
    def |@[M[_], R](f: (T1, T2, T3, T4, T5, T6, T7) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult7[T1, T2, T3, T4, T5, T6, T7]] =
      executeRowApply(f, execute)
    def |@>[M[_], R](f: (T1, T2, T3, T4, T5, T6, T7) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult7[T1, T2, T3, T4, T5, T6, T7]] =
      executeRowApply(f, true)
    def |*[R](f: (T1, T2, T3, T4, T5, T6, T7) => R)(implicit asResult: AsResult[R]): ExecutorService => DataTableResult7[T1, T2, T3, T4, T5, T6, T7] = (es: ExecutorService) =>
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7) => Future.fork(Future.delay(f(t1,t2,t3,t4,t5,t6,t7)))(es), execute)(asResult, control.FutureInstances.parallelApplicative).run
    def |*>[R](f: (T1, T2, T3, T4, T5, T6, T7) => R)(implicit asResult: AsResult[R]): ExecutorService => DataTableResult7[T1, T2, T3, T4, T5, T6, T7] = (es: ExecutorService) =>
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7) => Future.fork(Future.delay(f(t1,t2,t3,t4,t5,t6,t7)))(es), true)(asResult, control.FutureInstances.parallelApplicative).run
    def executeRowApply[R, M[_]](f: (T1, T2, T3, T4, T5, T6, T7) => M[R], exec: Boolean)(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult7[T1, T2, T3, T4, T5, T6, T7]] = {
      if (exec)
        app.map(app.traverse(rows.toList) { d: DataRow7[T1, T2, T3, T4, T5, T6, T7] =>
          app.map(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7))(r => (d, AsResult(r).execute))
        })(rs => DataTableResult7[T1, T2, T3, T4, T5, T6, T7](titles, rs))
      else app.pure(DataTableResult7[T1, T2, T3, T4, T5, T6, T7](titles, List[(DataRow7[T1, T2, T3, T4, T5, T6, T7], Result)]()))
    }
  }
  case class Table8[T1, T2, T3, T4, T5, T6, T7, T8](override val titles: List[String], rows: List[DataRow8[T1, T2, T3, T4, T5, T6, T7, T8]], override val execute: Boolean = false) extends Table(titles, execute) { outer =>
    def |[S1 >: T1, S2 >: T2, S3 >: T3, S4 >: T4, S5 >: T5, S6 >: T6, S7 >: T7, S8 >: T8](row: DataRow8[S1, S2, S3, S4, S5, S6, S7, S8]) = Table8(titles, outer.rows :+ row, execute)
    def |[R : AsResult](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R) = executeRow(f, execute)
    def |>[R : AsResult](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R) = executeRow(f, true)
    def executeRow[R : AsResult](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R, exec: Boolean): DataTableResult8[T1, T2, T3, T4, T5, T6, T7, T8] = {
      if (exec) DataTableResult8[T1, T2, T3, T4, T5, T6, T7, T8](titles, rows map { d => (d, AsResult(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7,d.t8)).execute) }).check
      else      DataTableResult8[T1, T2, T3, T4, T5, T6, T7, T8](titles, List[(DataRow8[T1, T2, T3, T4, T5, T6, T7, T8], Result)]())
    }
    def |@[M[_], R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult8[T1, T2, T3, T4, T5, T6, T7, T8]] =
      executeRowApply(f, execute)
    def |@>[M[_], R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult8[T1, T2, T3, T4, T5, T6, T7, T8]] =
      executeRowApply(f, true)
    def |*[R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(implicit asResult: AsResult[R]): ExecutorService => DataTableResult8[T1, T2, T3, T4, T5, T6, T7, T8] = (es: ExecutorService) =>
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8) => Future.fork(Future.delay(f(t1,t2,t3,t4,t5,t6,t7,t8)))(es), execute)(asResult, control.FutureInstances.parallelApplicative).run
    def |*>[R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(implicit asResult: AsResult[R]): ExecutorService => DataTableResult8[T1, T2, T3, T4, T5, T6, T7, T8] = (es: ExecutorService) =>
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8) => Future.fork(Future.delay(f(t1,t2,t3,t4,t5,t6,t7,t8)))(es), true)(asResult, control.FutureInstances.parallelApplicative).run
    def executeRowApply[R, M[_]](f: (T1, T2, T3, T4, T5, T6, T7, T8) => M[R], exec: Boolean)(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult8[T1, T2, T3, T4, T5, T6, T7, T8]] = {
      if (exec)
        app.map(app.traverse(rows.toList) { d: DataRow8[T1, T2, T3, T4, T5, T6, T7, T8] =>
          app.map(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7,d.t8))(r => (d, AsResult(r).execute))
        })(rs => DataTableResult8[T1, T2, T3, T4, T5, T6, T7, T8](titles, rs))
      else app.pure(DataTableResult8[T1, T2, T3, T4, T5, T6, T7, T8](titles, List[(DataRow8[T1, T2, T3, T4, T5, T6, T7, T8], Result)]()))
    }
  }
  case class Table9[T1, T2, T3, T4, T5, T6, T7, T8, T9](override val titles: List[String], rows: List[DataRow9[T1, T2, T3, T4, T5, T6, T7, T8, T9]], override val execute: Boolean = false) extends Table(titles, execute) { outer =>
    def |[S1 >: T1, S2 >: T2, S3 >: T3, S4 >: T4, S5 >: T5, S6 >: T6, S7 >: T7, S8 >: T8, S9 >: T9](row: DataRow9[S1, S2, S3, S4, S5, S6, S7, S8, S9]) = Table9(titles, outer.rows :+ row, execute)
    def |[R : AsResult](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R) = executeRow(f, execute)
    def |>[R : AsResult](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R) = executeRow(f, true)
    def executeRow[R : AsResult](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R, exec: Boolean): DataTableResult9[T1, T2, T3, T4, T5, T6, T7, T8, T9] = {
      if (exec) DataTableResult9[T1, T2, T3, T4, T5, T6, T7, T8, T9](titles, rows map { d => (d, AsResult(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7,d.t8,d.t9)).execute) }).check
      else      DataTableResult9[T1, T2, T3, T4, T5, T6, T7, T8, T9](titles, List[(DataRow9[T1, T2, T3, T4, T5, T6, T7, T8, T9], Result)]())
    }
    def |@[M[_], R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] =
      executeRowApply(f, execute)
    def |@>[M[_], R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] =
      executeRowApply(f, true)
    def |*[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R)(implicit asResult: AsResult[R]): ExecutorService => DataTableResult9[T1, T2, T3, T4, T5, T6, T7, T8, T9] = (es: ExecutorService) =>
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9) => Future.fork(Future.delay(f(t1,t2,t3,t4,t5,t6,t7,t8,t9)))(es), execute)(asResult, control.FutureInstances.parallelApplicative).run
    def |*>[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R)(implicit asResult: AsResult[R]): ExecutorService => DataTableResult9[T1, T2, T3, T4, T5, T6, T7, T8, T9] = (es: ExecutorService) =>
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9) => Future.fork(Future.delay(f(t1,t2,t3,t4,t5,t6,t7,t8,t9)))(es), true)(asResult, control.FutureInstances.parallelApplicative).run
    def executeRowApply[R, M[_]](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => M[R], exec: Boolean)(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = {
      if (exec)
        app.map(app.traverse(rows.toList) { d: DataRow9[T1, T2, T3, T4, T5, T6, T7, T8, T9] =>
          app.map(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7,d.t8,d.t9))(r => (d, AsResult(r).execute))
        })(rs => DataTableResult9[T1, T2, T3, T4, T5, T6, T7, T8, T9](titles, rs))
      else app.pure(DataTableResult9[T1, T2, T3, T4, T5, T6, T7, T8, T9](titles, List[(DataRow9[T1, T2, T3, T4, T5, T6, T7, T8, T9], Result)]()))
    }
  }
  case class Table10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](override val titles: List[String], rows: List[DataRow10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]], override val execute: Boolean = false) extends Table(titles, execute) { outer =>
    def |[S1 >: T1, S2 >: T2, S3 >: T3, S4 >: T4, S5 >: T5, S6 >: T6, S7 >: T7, S8 >: T8, S9 >: T9, S10 >: T10](row: DataRow10[S1, S2, S3, S4, S5, S6, S7, S8, S9, S10]) = Table10(titles, outer.rows :+ row, execute)
    def |[R : AsResult](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R) = executeRow(f, execute)
    def |>[R : AsResult](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R) = executeRow(f, true)
    def executeRow[R : AsResult](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R, exec: Boolean): DataTableResult10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = {
      if (exec) DataTableResult10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](titles, rows map { d => (d, AsResult(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7,d.t8,d.t9,d.t10)).execute) }).check
      else      DataTableResult10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](titles, List[(DataRow10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], Result)]())
    }
    def |@[M[_], R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] =
      executeRowApply(f, execute)
    def |@>[M[_], R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] =
      executeRowApply(f, true)
    def |*[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R)(implicit asResult: AsResult[R]): ExecutorService => DataTableResult10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = (es: ExecutorService) =>
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10) => Future.fork(Future.delay(f(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10)))(es), execute)(asResult, control.FutureInstances.parallelApplicative).run
    def |*>[R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R)(implicit asResult: AsResult[R]): ExecutorService => DataTableResult10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = (es: ExecutorService) =>
      executeRowApply((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10) => Future.fork(Future.delay(f(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10)))(es), true)(asResult, control.FutureInstances.parallelApplicative).run
    def executeRowApply[R, M[_]](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => M[R], exec: Boolean)(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = {
      if (exec)
        app.map(app.traverse(rows.toList) { d: DataRow10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] =>
          app.map(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7,d.t8,d.t9,d.t10))(r => (d, AsResult(r).execute))
        })(rs => DataTableResult10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](titles, rs))
      else app.pure(DataTableResult10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](titles, List[(DataRow10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], Result)]()))
    }
  }

  import DataTableResult._

  object DataTableResult {
    /** @return the logical and combination of all the results */
    def allSuccess[R : AsResult](results: List[(Seq[String], R)]): Result = {
      results.foldLeft(Success("", results.size): Result)((res, cur) => res and AsResult(cur._2))
    }
    /** @return the status of the row + the values + the failure message if any */
    def resultLine(line: Seq[String], result: Result): Seq[String] = {
      val message = if (result.isSuccess) "" else result.message
      result.status +: line :+ message
    }
  }


  case class DataTableResult1[T1](titles: List[String], rows: List[(DataRow1[T1], Result)], show1: T1 => String = (_:T1).notNull) {
    def as(show1: T1 => String = (_:T1).notNull) = copy(show1 = show1)

    def check =
      if (rows.map(_._2).forall(_.isSuccess) || !checkCanThrowException) this
      else throw new DataTableResultException1[T1](this)

    def result: DecoratedResult[DataTable] = {
      val results = rows.map { case (row, r) => (List(show1(row.t1)), r) }
      val rs = allSuccess(results)
      DecoratedResult(DataTable(titles, results), rs.updateMessage {
        TextTable("" +: titles :+ "", results.map { case (line, r) => resultLine(line, AsResult(r)) }:_*).show
      })
    }
  }

  case class DataTableResultException1[T1](r: DataTableResult1[T1]) extends Exception

  implicit class DataTableResultOps1[T1](r: =>DataTableResult1[T1]) {
    def showAs(show1: T1 => String = (_:T1).notNull) = {
      try r.as(show1)
      catch { case DataTableResultException1(result) => throw new DecoratedResultException(result.asInstanceOf[DataTableResult1[T1]].as(show1).result) }
    }
  }

  object DataTableResult1 {
    implicit def AsResultDataTableResult1[T1]: AsResult[DataTableResult1[T1]] = new AsResult[DataTableResult1[T1]] {
      def asResult(t: =>DataTableResult1[T1]): Result =
        checkResultFailure(t.result)
    }
  }


  case class DataTableResult2[T1, T2](titles: List[String], rows: List[(DataRow2[T1, T2], Result)], show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull) {
    def as(show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull) = copy(show1 = show1, show2 = show2)

    def check =
      if (rows.map(_._2).forall(_.isSuccess) || !checkCanThrowException) this
      else throw new DataTableResultException2[T1, T2](this)

    def result: DecoratedResult[DataTable] = {
      val results = rows.map { case (row, r) => (List(show1(row.t1),show2(row.t2)), r) }
      val rs = allSuccess(results)
      DecoratedResult(DataTable(titles, results), rs.updateMessage {
        TextTable("" +: titles :+ "", results.map { case (line, r) => resultLine(line, AsResult(r)) }:_*).show
      })
    }
  }

  case class DataTableResultException2[T1, T2](r: DataTableResult2[T1, T2]) extends Exception

  implicit class DataTableResultOps2[T1, T2](r: =>DataTableResult2[T1, T2]) {
    def showAs(show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull) = {
      try r.as(show1, show2)
      catch { case DataTableResultException2(result) => throw new DecoratedResultException(result.asInstanceOf[DataTableResult2[T1, T2]].as(show1, show2).result) }
    }
  }

  object DataTableResult2 {
    implicit def AsResultDataTableResult2[T1, T2]: AsResult[DataTableResult2[T1, T2]] = new AsResult[DataTableResult2[T1, T2]] {
      def asResult(t: =>DataTableResult2[T1, T2]): Result =
        checkResultFailure(t.result)
    }
  }


  case class DataTableResult3[T1, T2, T3](titles: List[String], rows: List[(DataRow3[T1, T2, T3], Result)], show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull) {
    def as(show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull) = copy(show1 = show1, show2 = show2, show3 = show3)

    def check =
      if (rows.map(_._2).forall(_.isSuccess) || !checkCanThrowException) this
      else throw new DataTableResultException3[T1, T2, T3](this)

    def result: DecoratedResult[DataTable] = {
      val results = rows.map { case (row, r) => (List(show1(row.t1),show2(row.t2),show3(row.t3)), r) }
      val rs = allSuccess(results)
      DecoratedResult(DataTable(titles, results), rs.updateMessage {
        TextTable("" +: titles :+ "", results.map { case (line, r) => resultLine(line, AsResult(r)) }:_*).show
      })
    }
  }

  case class DataTableResultException3[T1, T2, T3](r: DataTableResult3[T1, T2, T3]) extends Exception

  implicit class DataTableResultOps3[T1, T2, T3](r: =>DataTableResult3[T1, T2, T3]) {
    def showAs(show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull) = {
      try r.as(show1, show2, show3)
      catch { case DataTableResultException3(result) => throw new DecoratedResultException(result.asInstanceOf[DataTableResult3[T1, T2, T3]].as(show1, show2, show3).result) }
    }
  }

  object DataTableResult3 {
    implicit def AsResultDataTableResult3[T1, T2, T3]: AsResult[DataTableResult3[T1, T2, T3]] = new AsResult[DataTableResult3[T1, T2, T3]] {
      def asResult(t: =>DataTableResult3[T1, T2, T3]): Result =
        checkResultFailure(t.result)
    }
  }


  case class DataTableResult4[T1, T2, T3, T4](titles: List[String], rows: List[(DataRow4[T1, T2, T3, T4], Result)], show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull) {
    def as(show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull) = copy(show1 = show1, show2 = show2, show3 = show3, show4 = show4)

    def check =
      if (rows.map(_._2).forall(_.isSuccess) || !checkCanThrowException) this
      else throw new DataTableResultException4[T1, T2, T3, T4](this)

    def result: DecoratedResult[DataTable] = {
      val results = rows.map { case (row, r) => (List(show1(row.t1),show2(row.t2),show3(row.t3),show4(row.t4)), r) }
      val rs = allSuccess(results)
      DecoratedResult(DataTable(titles, results), rs.updateMessage {
        TextTable("" +: titles :+ "", results.map { case (line, r) => resultLine(line, AsResult(r)) }:_*).show
      })
    }
  }

  case class DataTableResultException4[T1, T2, T3, T4](r: DataTableResult4[T1, T2, T3, T4]) extends Exception

  implicit class DataTableResultOps4[T1, T2, T3, T4](r: =>DataTableResult4[T1, T2, T3, T4]) {
    def showAs(show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull) = {
      try r.as(show1, show2, show3, show4)
      catch { case DataTableResultException4(result) => throw new DecoratedResultException(result.asInstanceOf[DataTableResult4[T1, T2, T3, T4]].as(show1, show2, show3, show4).result) }
    }
  }

  object DataTableResult4 {
    implicit def AsResultDataTableResult4[T1, T2, T3, T4]: AsResult[DataTableResult4[T1, T2, T3, T4]] = new AsResult[DataTableResult4[T1, T2, T3, T4]] {
      def asResult(t: =>DataTableResult4[T1, T2, T3, T4]): Result =
        checkResultFailure(t.result)
    }
  }


  case class DataTableResult5[T1, T2, T3, T4, T5](titles: List[String], rows: List[(DataRow5[T1, T2, T3, T4, T5], Result)], show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull, show5: T5 => String = (_:T5).notNull) {
    def as(show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull, show5: T5 => String = (_:T5).notNull) = copy(show1 = show1, show2 = show2, show3 = show3, show4 = show4, show5 = show5)

    def check =
      if (rows.map(_._2).forall(_.isSuccess) || !checkCanThrowException) this
      else throw new DataTableResultException5[T1, T2, T3, T4, T5](this)

    def result: DecoratedResult[DataTable] = {
      val results = rows.map { case (row, r) => (List(show1(row.t1),show2(row.t2),show3(row.t3),show4(row.t4),show5(row.t5)), r) }
      val rs = allSuccess(results)
      DecoratedResult(DataTable(titles, results), rs.updateMessage {
        TextTable("" +: titles :+ "", results.map { case (line, r) => resultLine(line, AsResult(r)) }:_*).show
      })
    }
  }

  case class DataTableResultException5[T1, T2, T3, T4, T5](r: DataTableResult5[T1, T2, T3, T4, T5]) extends Exception

  implicit class DataTableResultOps5[T1, T2, T3, T4, T5](r: =>DataTableResult5[T1, T2, T3, T4, T5]) {
    def showAs(show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull, show5: T5 => String = (_:T5).notNull) = {
      try r.as(show1, show2, show3, show4, show5)
      catch { case DataTableResultException5(result) => throw new DecoratedResultException(result.asInstanceOf[DataTableResult5[T1, T2, T3, T4, T5]].as(show1, show2, show3, show4, show5).result) }
    }
  }

  object DataTableResult5 {
    implicit def AsResultDataTableResult5[T1, T2, T3, T4, T5]: AsResult[DataTableResult5[T1, T2, T3, T4, T5]] = new AsResult[DataTableResult5[T1, T2, T3, T4, T5]] {
      def asResult(t: =>DataTableResult5[T1, T2, T3, T4, T5]): Result =
        checkResultFailure(t.result)
    }
  }


  case class DataTableResult6[T1, T2, T3, T4, T5, T6](titles: List[String], rows: List[(DataRow6[T1, T2, T3, T4, T5, T6], Result)], show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull, show5: T5 => String = (_:T5).notNull, show6: T6 => String = (_:T6).notNull) {
    def as(show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull, show5: T5 => String = (_:T5).notNull, show6: T6 => String = (_:T6).notNull) = copy(show1 = show1, show2 = show2, show3 = show3, show4 = show4, show5 = show5, show6 = show6)

    def check =
      if (rows.map(_._2).forall(_.isSuccess) || !checkCanThrowException) this
      else throw new DataTableResultException6[T1, T2, T3, T4, T5, T6](this)

    def result: DecoratedResult[DataTable] = {
      val results = rows.map { case (row, r) => (List(show1(row.t1),show2(row.t2),show3(row.t3),show4(row.t4),show5(row.t5),show6(row.t6)), r) }
      val rs = allSuccess(results)
      DecoratedResult(DataTable(titles, results), rs.updateMessage {
        TextTable("" +: titles :+ "", results.map { case (line, r) => resultLine(line, AsResult(r)) }:_*).show
      })
    }
  }

  case class DataTableResultException6[T1, T2, T3, T4, T5, T6](r: DataTableResult6[T1, T2, T3, T4, T5, T6]) extends Exception

  implicit class DataTableResultOps6[T1, T2, T3, T4, T5, T6](r: =>DataTableResult6[T1, T2, T3, T4, T5, T6]) {
    def showAs(show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull, show5: T5 => String = (_:T5).notNull, show6: T6 => String = (_:T6).notNull) = {
      try r.as(show1, show2, show3, show4, show5, show6)
      catch { case DataTableResultException6(result) => throw new DecoratedResultException(result.asInstanceOf[DataTableResult6[T1, T2, T3, T4, T5, T6]].as(show1, show2, show3, show4, show5, show6).result) }
    }
  }

  object DataTableResult6 {
    implicit def AsResultDataTableResult6[T1, T2, T3, T4, T5, T6]: AsResult[DataTableResult6[T1, T2, T3, T4, T5, T6]] = new AsResult[DataTableResult6[T1, T2, T3, T4, T5, T6]] {
      def asResult(t: =>DataTableResult6[T1, T2, T3, T4, T5, T6]): Result =
        checkResultFailure(t.result)
    }
  }


  case class DataTableResult7[T1, T2, T3, T4, T5, T6, T7](titles: List[String], rows: List[(DataRow7[T1, T2, T3, T4, T5, T6, T7], Result)], show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull, show5: T5 => String = (_:T5).notNull, show6: T6 => String = (_:T6).notNull, show7: T7 => String = (_:T7).notNull) {
    def as(show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull, show5: T5 => String = (_:T5).notNull, show6: T6 => String = (_:T6).notNull, show7: T7 => String = (_:T7).notNull) = copy(show1 = show1, show2 = show2, show3 = show3, show4 = show4, show5 = show5, show6 = show6, show7 = show7)

    def check =
      if (rows.map(_._2).forall(_.isSuccess) || !checkCanThrowException) this
      else throw new DataTableResultException7[T1, T2, T3, T4, T5, T6, T7](this)

    def result: DecoratedResult[DataTable] = {
      val results = rows.map { case (row, r) => (List(show1(row.t1),show2(row.t2),show3(row.t3),show4(row.t4),show5(row.t5),show6(row.t6),show7(row.t7)), r) }
      val rs = allSuccess(results)
      DecoratedResult(DataTable(titles, results), rs.updateMessage {
        TextTable("" +: titles :+ "", results.map { case (line, r) => resultLine(line, AsResult(r)) }:_*).show
      })
    }
  }

  case class DataTableResultException7[T1, T2, T3, T4, T5, T6, T7](r: DataTableResult7[T1, T2, T3, T4, T5, T6, T7]) extends Exception

  implicit class DataTableResultOps7[T1, T2, T3, T4, T5, T6, T7](r: =>DataTableResult7[T1, T2, T3, T4, T5, T6, T7]) {
    def showAs(show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull, show5: T5 => String = (_:T5).notNull, show6: T6 => String = (_:T6).notNull, show7: T7 => String = (_:T7).notNull) = {
      try r.as(show1, show2, show3, show4, show5, show6, show7)
      catch { case DataTableResultException7(result) => throw new DecoratedResultException(result.asInstanceOf[DataTableResult7[T1, T2, T3, T4, T5, T6, T7]].as(show1, show2, show3, show4, show5, show6, show7).result) }
    }
  }

  object DataTableResult7 {
    implicit def AsResultDataTableResult7[T1, T2, T3, T4, T5, T6, T7]: AsResult[DataTableResult7[T1, T2, T3, T4, T5, T6, T7]] = new AsResult[DataTableResult7[T1, T2, T3, T4, T5, T6, T7]] {
      def asResult(t: =>DataTableResult7[T1, T2, T3, T4, T5, T6, T7]): Result =
        checkResultFailure(t.result)
    }
  }


  case class DataTableResult8[T1, T2, T3, T4, T5, T6, T7, T8](titles: List[String], rows: List[(DataRow8[T1, T2, T3, T4, T5, T6, T7, T8], Result)], show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull, show5: T5 => String = (_:T5).notNull, show6: T6 => String = (_:T6).notNull, show7: T7 => String = (_:T7).notNull, show8: T8 => String = (_:T8).notNull) {
    def as(show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull, show5: T5 => String = (_:T5).notNull, show6: T6 => String = (_:T6).notNull, show7: T7 => String = (_:T7).notNull, show8: T8 => String = (_:T8).notNull) = copy(show1 = show1, show2 = show2, show3 = show3, show4 = show4, show5 = show5, show6 = show6, show7 = show7, show8 = show8)

    def check =
      if (rows.map(_._2).forall(_.isSuccess) || !checkCanThrowException) this
      else throw new DataTableResultException8[T1, T2, T3, T4, T5, T6, T7, T8](this)

    def result: DecoratedResult[DataTable] = {
      val results = rows.map { case (row, r) => (List(show1(row.t1),show2(row.t2),show3(row.t3),show4(row.t4),show5(row.t5),show6(row.t6),show7(row.t7),show8(row.t8)), r) }
      val rs = allSuccess(results)
      DecoratedResult(DataTable(titles, results), rs.updateMessage {
        TextTable("" +: titles :+ "", results.map { case (line, r) => resultLine(line, AsResult(r)) }:_*).show
      })
    }
  }

  case class DataTableResultException8[T1, T2, T3, T4, T5, T6, T7, T8](r: DataTableResult8[T1, T2, T3, T4, T5, T6, T7, T8]) extends Exception

  implicit class DataTableResultOps8[T1, T2, T3, T4, T5, T6, T7, T8](r: =>DataTableResult8[T1, T2, T3, T4, T5, T6, T7, T8]) {
    def showAs(show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull, show5: T5 => String = (_:T5).notNull, show6: T6 => String = (_:T6).notNull, show7: T7 => String = (_:T7).notNull, show8: T8 => String = (_:T8).notNull) = {
      try r.as(show1, show2, show3, show4, show5, show6, show7, show8)
      catch { case DataTableResultException8(result) => throw new DecoratedResultException(result.asInstanceOf[DataTableResult8[T1, T2, T3, T4, T5, T6, T7, T8]].as(show1, show2, show3, show4, show5, show6, show7, show8).result) }
    }
  }

  object DataTableResult8 {
    implicit def AsResultDataTableResult8[T1, T2, T3, T4, T5, T6, T7, T8]: AsResult[DataTableResult8[T1, T2, T3, T4, T5, T6, T7, T8]] = new AsResult[DataTableResult8[T1, T2, T3, T4, T5, T6, T7, T8]] {
      def asResult(t: =>DataTableResult8[T1, T2, T3, T4, T5, T6, T7, T8]): Result =
        checkResultFailure(t.result)
    }
  }


  case class DataTableResult9[T1, T2, T3, T4, T5, T6, T7, T8, T9](titles: List[String], rows: List[(DataRow9[T1, T2, T3, T4, T5, T6, T7, T8, T9], Result)], show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull, show5: T5 => String = (_:T5).notNull, show6: T6 => String = (_:T6).notNull, show7: T7 => String = (_:T7).notNull, show8: T8 => String = (_:T8).notNull, show9: T9 => String = (_:T9).notNull) {
    def as(show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull, show5: T5 => String = (_:T5).notNull, show6: T6 => String = (_:T6).notNull, show7: T7 => String = (_:T7).notNull, show8: T8 => String = (_:T8).notNull, show9: T9 => String = (_:T9).notNull) = copy(show1 = show1, show2 = show2, show3 = show3, show4 = show4, show5 = show5, show6 = show6, show7 = show7, show8 = show8, show9 = show9)

    def check =
      if (rows.map(_._2).forall(_.isSuccess) || !checkCanThrowException) this
      else throw new DataTableResultException9[T1, T2, T3, T4, T5, T6, T7, T8, T9](this)

    def result: DecoratedResult[DataTable] = {
      val results = rows.map { case (row, r) => (List(show1(row.t1),show2(row.t2),show3(row.t3),show4(row.t4),show5(row.t5),show6(row.t6),show7(row.t7),show8(row.t8),show9(row.t9)), r) }
      val rs = allSuccess(results)
      DecoratedResult(DataTable(titles, results), rs.updateMessage {
        TextTable("" +: titles :+ "", results.map { case (line, r) => resultLine(line, AsResult(r)) }:_*).show
      })
    }
  }

  case class DataTableResultException9[T1, T2, T3, T4, T5, T6, T7, T8, T9](r: DataTableResult9[T1, T2, T3, T4, T5, T6, T7, T8, T9]) extends Exception

  implicit class DataTableResultOps9[T1, T2, T3, T4, T5, T6, T7, T8, T9](r: =>DataTableResult9[T1, T2, T3, T4, T5, T6, T7, T8, T9]) {
    def showAs(show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull, show5: T5 => String = (_:T5).notNull, show6: T6 => String = (_:T6).notNull, show7: T7 => String = (_:T7).notNull, show8: T8 => String = (_:T8).notNull, show9: T9 => String = (_:T9).notNull) = {
      try r.as(show1, show2, show3, show4, show5, show6, show7, show8, show9)
      catch { case DataTableResultException9(result) => throw new DecoratedResultException(result.asInstanceOf[DataTableResult9[T1, T2, T3, T4, T5, T6, T7, T8, T9]].as(show1, show2, show3, show4, show5, show6, show7, show8, show9).result) }
    }
  }

  object DataTableResult9 {
    implicit def AsResultDataTableResult9[T1, T2, T3, T4, T5, T6, T7, T8, T9]: AsResult[DataTableResult9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = new AsResult[DataTableResult9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] {
      def asResult(t: =>DataTableResult9[T1, T2, T3, T4, T5, T6, T7, T8, T9]): Result =
        checkResultFailure(t.result)
    }
  }


  case class DataTableResult10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](titles: List[String], rows: List[(DataRow10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], Result)], show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull, show5: T5 => String = (_:T5).notNull, show6: T6 => String = (_:T6).notNull, show7: T7 => String = (_:T7).notNull, show8: T8 => String = (_:T8).notNull, show9: T9 => String = (_:T9).notNull, show10: T10 => String = (_:T10).notNull) {
    def as(show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull, show5: T5 => String = (_:T5).notNull, show6: T6 => String = (_:T6).notNull, show7: T7 => String = (_:T7).notNull, show8: T8 => String = (_:T8).notNull, show9: T9 => String = (_:T9).notNull, show10: T10 => String = (_:T10).notNull) = copy(show1 = show1, show2 = show2, show3 = show3, show4 = show4, show5 = show5, show6 = show6, show7 = show7, show8 = show8, show9 = show9, show10 = show10)

    def check =
      if (rows.map(_._2).forall(_.isSuccess) || !checkCanThrowException) this
      else throw new DataTableResultException10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](this)

    def result: DecoratedResult[DataTable] = {
      val results = rows.map { case (row, r) => (List(show1(row.t1),show2(row.t2),show3(row.t3),show4(row.t4),show5(row.t5),show6(row.t6),show7(row.t7),show8(row.t8),show9(row.t9),show10(row.t10)), r) }
      val rs = allSuccess(results)
      DecoratedResult(DataTable(titles, results), rs.updateMessage {
        TextTable("" +: titles :+ "", results.map { case (line, r) => resultLine(line, AsResult(r)) }:_*).show
      })
    }
  }

  case class DataTableResultException10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](r: DataTableResult10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]) extends Exception

  implicit class DataTableResultOps10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](r: =>DataTableResult10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]) {
    def showAs(show1: T1 => String = (_:T1).notNull, show2: T2 => String = (_:T2).notNull, show3: T3 => String = (_:T3).notNull, show4: T4 => String = (_:T4).notNull, show5: T5 => String = (_:T5).notNull, show6: T6 => String = (_:T6).notNull, show7: T7 => String = (_:T7).notNull, show8: T8 => String = (_:T8).notNull, show9: T9 => String = (_:T9).notNull, show10: T10 => String = (_:T10).notNull) = {
      try r.as(show1, show2, show3, show4, show5, show6, show7, show8, show9, show10)
      catch { case DataTableResultException10(result) => throw new DecoratedResultException(result.asInstanceOf[DataTableResult10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]].as(show1, show2, show3, show4, show5, show6, show7, show8, show9, show10).result) }
    }
  }

  object DataTableResult10 {
    implicit def AsResultDataTableResult10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]: AsResult[DataTableResult10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = new AsResult[DataTableResult10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] {
      def asResult(t: =>DataTableResult10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]): Result =
        checkResultFailure(t.result)
    }
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
      dataResult,
      dataResults(n),
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

  def dataResult: String =
    """|import DataTableResult._
       |
       |object DataTableResult {
       |  /** @return the logical and combination of all the results */
       |  def allSuccess[R : AsResult](results: List[(Seq[String], R)]): Result = {
       |    results.foldLeft(Success("", results.size): Result)((res, cur) => res and AsResult(cur._2))
       |  }
       |  /** @return the status of the row + the values + the failure message if any */
       |  def resultLine(line: Seq[String], result: Result): Seq[String] = {
       |    val message = if (result.isSuccess) "" else result.message
       |    result.status +: line :+ message
       |  }
       |}
       |""".stripMargin

  def dataResults(n: Int): String = (1 to n).map { i =>
    s"""|  case class DataTableResult${i+types(i)}(titles: List[String], rows: List[(DataRow${i+types(i)}, Result)], ${(1 to i).map(j => s"show$j: T$j => String = (_:T$j).notNull").mkString(", ")}) {
        |    def as(${(1 to i).map(j => s"show$j: T$j => String = (_:T$j).notNull").mkString(", ")}) = copy(${(1 to i).map(j => s"show$j = show$j").mkString(", ")})
        |
        |    def check =
        |      if (rows.map(_._2).forall(_.isSuccess) || !checkCanThrowException) this
        |      else throw new DataTableResultException${i+types(i)}(this)
        |
        |    def result: DecoratedResult[DataTable] = {
        |      val results = rows.map { case (row, r) => (List(${(1 to i).map(j => s"show$j(row.t$j)").mkString(",")}), r) }
        |      val rs = allSuccess(results)
        |      DecoratedResult(DataTable(titles, results), rs.updateMessage {
        |        TextTable("" +: titles :+ "", results.map { case (line, r) => resultLine(line, AsResult(r)) }:_*).show
        |      })
        |    }
        |  }
        |
        |  case class DataTableResultException${i+types(i)}(r: DataTableResult${i+types(i)}) extends Exception
        |
        |  implicit class DataTableResultOps${i+types(i)}(r: =>DataTableResult${i+types(i)}) {
        |    def showAs(${(1 to i).map(j => s"show$j: T$j => String = (_:T$j).notNull").mkString(", ")}) = {
        |      try r.as(${(1 to i).map(j => s"show$j").mkString(", ")})
        |      catch { case DataTableResultException$i(result) => throw new DecoratedResultException(result.asInstanceOf[DataTableResult${i+types(i)}].as(${(1 to i).map(j => s"show$j").mkString(", ")}).result) }
        |    }
        |  }
        |
        |  object DataTableResult$i {
        |    implicit def AsResultDataTableResult${i+types(i)}: AsResult[DataTableResult${i+types(i)}] = new AsResult[DataTableResult${i+types(i)}] {
        |      def asResult(t: =>DataTableResult${i+types(i)}): Result =
        |        checkResultFailure(t.result)
        |    }
        |  }
        |
        |""".stripMargin
  }.mkString("\n")


  def tableClasses(n: Int) = {
    (1 to n).map { i =>
      List("case class Table"+i+types(i)+"(override val titles: List[String], rows: List["+dataRow(i)+"], override val execute: Boolean = false) extends "+
        "Table(titles, execute) { outer =>",
        "  def |"+st(i)+"(row: "+dataRow(i, letter="S")+") = "+table(i)+"(titles, outer.rows :+ row, execute)",
        "  def |[R : AsResult](f: "+typesTuple(i)+" => R) = executeRow(f, execute)",
        "  def |>[R : AsResult](f: "+typesTuple(i)+" => R) = executeRow(f, true)",
        "  def executeRow[R : AsResult](f: "+typesTuple(i)+" => R, exec: Boolean): DataTableResult"+i+types(i)+" = {",
        "    if (exec) DataTableResult"+i+types(i)+"(titles, rows map { d => (d, AsResult(f("+(1 to i).map("d.t"+_).mkString(",")+")).execute) }).check",
        "    else      DataTableResult"+i+types(i)+"(titles, List[(DataRow"+i+types(i)+", Result)]())",
        "  }",
        "  def |@[M[_], R](f: "+typesTuple(i)+" => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult"+i+types(i)+"] =",
        "    executeRowApply(f, execute)",
        "  def |@>[M[_], R](f: "+typesTuple(i)+" => M[R])(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult"+i+types(i)+"] =",
        "    executeRowApply(f, true)",
        "  def |*[R](f: "+typesTuple(i)+" => R)(implicit asResult: AsResult[R]): ExecutorService => DataTableResult"+i+types(i)+" = (es: ExecutorService) => ",
        "    executeRowApply("+parametersList(i: Int)+" => Future.fork(Future.delay(f("+(1 to i).map("t"+_).mkString(",")+")))(es), execute)(asResult, control.FutureInstances.parallelApplicative).run",
        "  def |*>[R](f: "+typesTuple(i)+" => R)(implicit asResult: AsResult[R]): ExecutorService => DataTableResult"+i+types(i)+" = (es: ExecutorService) => ",
        "    executeRowApply("+parametersList(i: Int)+" => Future.fork(Future.delay(f("+(1 to i).map("t"+_).mkString(",")+")))(es), true)(asResult, control.FutureInstances.parallelApplicative).run",
        "  def executeRowApply[R, M[_]](f: "+typesTuple(i)+" => M[R], exec: Boolean)(implicit asResult: AsResult[R], app: Applicative[M]): M[DataTableResult"+i+types(i)+"] = {",
        "    if (exec)",
        "      app.map(app.traverse(rows.toList) { d: DataRow"+i+types(i)+" =>",
        "        app.map(f("+(1 to i).map("d.t"+_).mkString(",")+"))(r => (d, AsResult(r).execute))",
        "      })(rs => DataTableResult"+i+types(i)+"(titles, rs))",
        "    else app.pure(DataTableResult"+i+types(i)+"(titles, List[(DataRow"+i+types(i)+", Result)]()))",
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
  def types(i: Int) = typesAsList(i).mkString("[",", ", "]")
  def st(i: Int) = (1 to i).map(j => "S"+j+" >: T"+j).mkString("[",", ", "]")
  def typesAsList(i: Int, letter: String = "T"): Seq[String] =  (1 to i).map(letter+_)
  def typesList(i: Int): String = typesAsList(i).mkString(", ")
  def typesList(i: Int, n: Int): String =  (List(typesList(i)) ::: (i+1 to n).map(t => "Any").toList).mkString(", ")
  def typesTuple(i: Int) =  typesAsList(i).mkString("(",", ", ")")
  def dataRowDecl(i: Int, letter: String = "T") = "DataRow"+i+variantTypes(i, letter)
  def dataRow(i: Int, letter: String = "T") = "DataRow"+i+typesAsList(i, letter).mkString("[",", ", "]")
  def table(i: Int) = "Table"+i
}

