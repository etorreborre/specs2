package org.specs2
package matcher

import execute._
import ResultExecution._
import main.Arguments
import scala.xml.NodeSeq
import text.Trim._

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
trait DataTables {
  
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
     * @return the header as a | separated string
     */
	  def showTitles = titles.mkString("|", "|", "|")

	  /**
	   * Collect the results of each row
	   * @param results list of (row description, row execution result)
     * @return an aggregated Result from a list of results
     */
    protected def collect[R <% Result](results: List[(String, R)]): DecoratedResult[DataTable] = {
	    val result = allSuccess(results)
	    val header = result match {
	   	  case Success(_) => showTitles
	   	  case other      => "  " + showTitles  
	    }
      DecoratedResult(DataTable(titles, results), result.updateMessage {
	      header+"\n"+
        results.map((cur: (String, R)) => resultLine(cur._1, cur._2)).mkString("\n")
	    })
	  }
	  /** @return the logical and combination of all the results */
    private def allSuccess[R <% Result](results: List[(String, R)]): Result = {
      results.foldLeft(Success("", results.size): Result)((res, cur) => res and cur._2)
    }
    /** @return the status of the row + the values + the failure message if any */
	  private def resultLine(desc: String, result: Result): String = {
	    result.status+" "+desc+{
	   	  result match {
	   	    case Success(_) => ""
	   	    case _ => " " + result.message
	   	  }
	    }
	  }
  }
  /** GENERATED */
  case class TableHeader(titles: List[String]) {
    def ||(title: String) = copy(titles = this.titles :+ title)
    def |(title: String) = copy(titles = this.titles :+ title)
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
    def |(row: DataRow1[T1]) = Table1(titles, outer.rows :+ row, execute)
    def |[R <% Result](f: (T1) => R) = executeRow(f, execute)
    def |>[R <% Result](f: (T1) => R) = executeRow(f, true)
    def executeRow[R <% Result](f: (T1) => R, exec: Boolean): DecoratedResult[DataTable] = {
      if (exec)
        collect(rows map { (d: DataRow1[T1]) => (d.show, implicitly[R => Result].apply(f(d.t1)).execute) })
      else DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok"))
    }
  }
  case class Table2[T1, T2](override val titles: List[String], rows: List[DataRow2[T1, T2]], override val execute: Boolean = false) extends Table(titles, execute) { outer =>
    def |(row: DataRow2[T1, T2]) = Table2(titles, outer.rows :+ row, execute)
    def |[R <% Result](f: (T1, T2) => R) = executeRow(f, execute)
    def |>[R <% Result](f: (T1, T2) => R) = executeRow(f, true)
    def executeRow[R <% Result](f: (T1, T2) => R, exec: Boolean): DecoratedResult[DataTable] = {
      if (exec)
        collect(rows map { (d: DataRow2[T1, T2]) => (d.show, implicitly[R => Result].apply(f(d.t1,d.t2)).execute) })
      else DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok"))
    }
  }
  case class Table3[T1, T2, T3](override val titles: List[String], rows: List[DataRow3[T1, T2, T3]], override val execute: Boolean = false) extends Table(titles, execute) { outer =>
    def |(row: DataRow3[T1, T2, T3]) = Table3(titles, outer.rows :+ row, execute)
    def |[R <% Result](f: (T1, T2, T3) => R) = executeRow(f, execute)
    def |>[R <% Result](f: (T1, T2, T3) => R) = executeRow(f, true)
    def executeRow[R <% Result](f: (T1, T2, T3) => R, exec: Boolean): DecoratedResult[DataTable] = {
      if (exec)
        collect(rows map { (d: DataRow3[T1, T2, T3]) => (d.show, implicitly[R => Result].apply(f(d.t1,d.t2,d.t3)).execute) })
      else DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok"))
    }
  }
  case class Table4[T1, T2, T3, T4](override val titles: List[String], rows: List[DataRow4[T1, T2, T3, T4]], override val execute: Boolean = false) extends Table(titles, execute) { outer =>
    def |(row: DataRow4[T1, T2, T3, T4]) = Table4(titles, outer.rows :+ row, execute)
    def |[R <% Result](f: (T1, T2, T3, T4) => R) = executeRow(f, execute)
    def |>[R <% Result](f: (T1, T2, T3, T4) => R) = executeRow(f, true)
    def executeRow[R <% Result](f: (T1, T2, T3, T4) => R, exec: Boolean): DecoratedResult[DataTable] = {
      if (exec)
        collect(rows map { (d: DataRow4[T1, T2, T3, T4]) => (d.show, implicitly[R => Result].apply(f(d.t1,d.t2,d.t3,d.t4)).execute) })
      else DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok"))
    }
  }
  case class Table5[T1, T2, T3, T4, T5](override val titles: List[String], rows: List[DataRow5[T1, T2, T3, T4, T5]], override val execute: Boolean = false) extends Table(titles, execute) { outer =>
    def |(row: DataRow5[T1, T2, T3, T4, T5]) = Table5(titles, outer.rows :+ row, execute)
    def |[R <% Result](f: (T1, T2, T3, T4, T5) => R) = executeRow(f, execute)
    def |>[R <% Result](f: (T1, T2, T3, T4, T5) => R) = executeRow(f, true)
    def executeRow[R <% Result](f: (T1, T2, T3, T4, T5) => R, exec: Boolean): DecoratedResult[DataTable] = {
      if (exec)
        collect(rows map { (d: DataRow5[T1, T2, T3, T4, T5]) => (d.show, implicitly[R => Result].apply(f(d.t1,d.t2,d.t3,d.t4,d.t5)).execute) })
      else DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok"))
    }
  }
  case class Table6[T1, T2, T3, T4, T5, T6](override val titles: List[String], rows: List[DataRow6[T1, T2, T3, T4, T5, T6]], override val execute: Boolean = false) extends Table(titles, execute) { outer =>
    def |(row: DataRow6[T1, T2, T3, T4, T5, T6]) = Table6(titles, outer.rows :+ row, execute)
    def |[R <% Result](f: (T1, T2, T3, T4, T5, T6) => R) = executeRow(f, execute)
    def |>[R <% Result](f: (T1, T2, T3, T4, T5, T6) => R) = executeRow(f, true)
    def executeRow[R <% Result](f: (T1, T2, T3, T4, T5, T6) => R, exec: Boolean): DecoratedResult[DataTable] = {
      if (exec)
        collect(rows map { (d: DataRow6[T1, T2, T3, T4, T5, T6]) => (d.show, implicitly[R => Result].apply(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6)).execute) })
      else DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok"))
    }
  }
  case class Table7[T1, T2, T3, T4, T5, T6, T7](override val titles: List[String], rows: List[DataRow7[T1, T2, T3, T4, T5, T6, T7]], override val execute: Boolean = false) extends Table(titles, execute) { outer =>
    def |(row: DataRow7[T1, T2, T3, T4, T5, T6, T7]) = Table7(titles, outer.rows :+ row, execute)
    def |[R <% Result](f: (T1, T2, T3, T4, T5, T6, T7) => R) = executeRow(f, execute)
    def |>[R <% Result](f: (T1, T2, T3, T4, T5, T6, T7) => R) = executeRow(f, true)
    def executeRow[R <% Result](f: (T1, T2, T3, T4, T5, T6, T7) => R, exec: Boolean): DecoratedResult[DataTable] = {
      if (exec)
        collect(rows map { (d: DataRow7[T1, T2, T3, T4, T5, T6, T7]) => (d.show, implicitly[R => Result].apply(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7)).execute) })
      else DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok"))
    }
  }
  case class Table8[T1, T2, T3, T4, T5, T6, T7, T8](override val titles: List[String], rows: List[DataRow8[T1, T2, T3, T4, T5, T6, T7, T8]], override val execute: Boolean = false) extends Table(titles, execute) { outer =>
    def |(row: DataRow8[T1, T2, T3, T4, T5, T6, T7, T8]) = Table8(titles, outer.rows :+ row, execute)
    def |[R <% Result](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R) = executeRow(f, execute)
    def |>[R <% Result](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R) = executeRow(f, true)
    def executeRow[R <% Result](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R, exec: Boolean): DecoratedResult[DataTable] = {
      if (exec)
        collect(rows map { (d: DataRow8[T1, T2, T3, T4, T5, T6, T7, T8]) => (d.show, implicitly[R => Result].apply(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7,d.t8)).execute) })
      else DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok"))
    }
  }
  case class Table9[T1, T2, T3, T4, T5, T6, T7, T8, T9](override val titles: List[String], rows: List[DataRow9[T1, T2, T3, T4, T5, T6, T7, T8, T9]], override val execute: Boolean = false) extends Table(titles, execute) { outer =>
    def |(row: DataRow9[T1, T2, T3, T4, T5, T6, T7, T8, T9]) = Table9(titles, outer.rows :+ row, execute)
    def |[R <% Result](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R) = executeRow(f, execute)
    def |>[R <% Result](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R) = executeRow(f, true)
    def executeRow[R <% Result](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R, exec: Boolean): DecoratedResult[DataTable] = {
      if (exec)
        collect(rows map { (d: DataRow9[T1, T2, T3, T4, T5, T6, T7, T8, T9]) => (d.show, implicitly[R => Result].apply(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7,d.t8,d.t9)).execute) })
      else DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok"))
    }
  }
  case class Table10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](override val titles: List[String], rows: List[DataRow10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]], override val execute: Boolean = false) extends Table(titles, execute) { outer =>
    def |(row: DataRow10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]) = Table10(titles, outer.rows :+ row, execute)
    def |[R <% Result](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R) = executeRow(f, execute)
    def |>[R <% Result](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R) = executeRow(f, true)
    def executeRow[R <% Result](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R, exec: Boolean): DecoratedResult[DataTable] = {
      if (exec)
        collect(rows map { (d: DataRow10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]) => (d.show, implicitly[R => Result].apply(f(d.t1,d.t2,d.t3,d.t4,d.t5,d.t6,d.t7,d.t8,d.t9,d.t10)).execute) })
      else DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success("ok"))
    }
  }

  abstract class DataRow[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10] extends Product {
    def show = productIterator.mkString("|", "|", "|")
  }

  case class DataRow1[T1](t1: T1) extends DataRow[T1, Any, Any, Any, Any, Any, Any, Any, Any, Any] {
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
}
object DataTable {
  def apply[R <% Result](titles: Seq[String], results: Seq[(String, R)]): DataTable = DataTable(titles, results.collect { case (v, r) => DataTableRow(v, r) })
}
case class DataTableRow(cells: Seq[String], result: Result) {
  def isSuccess = result.isSuccess
}
object DataTableRow {
  def apply[R](values: String, result: R)(implicit convert: R => Result): DataTableRow = DataTableRow(values.trimEnclosing("|").splitTrim("\\|"), convert(result))
}

private[specs2]
object DataTables extends DataTables
/**
private object DataTablesGenerator {
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
      List("case class Table"+i+types(i)+"(override val titles: List[String], rows: List["+dataRow(i)+"], override val execute: Boolean = false) extends "+ 
             "Table(titles, execute) { outer =>",
           "  def |(row: "+dataRow(i)+") = "+table(i)+"(titles, outer.rows :+ row, execute)",
           "  def |[R <% Result](f: "+typesTuple(i)+" => R) = executeRow(f, execute)",
           "  def |>[R <% Result](f: "+typesTuple(i)+" => R) = executeRow(f, true)",
           "  def executeRow[R <% Result](f: "+typesTuple(i)+" => R, exec: Boolean): DecoratedResult[DataTable] = {",
           "    if (exec)",
           "      collect(rows map { (d: "+dataRow(i)+") => (d.show, implicitly[R => Result].apply(f("+(1 to i).map("d.t"+_).mkString(",")+")).execute) })",
           "    else DecoratedResult(DataTable(titles, Seq[DataTableRow]()), Success(\"ok\"))",
           "  }",
           "}").mkString("\n")    
    }.mkString("\n")
  }
  def dataRowClass(n: Int) = {
    "abstract class DataRow"+types(n)+" extends Product {\n"+
    "  def show = productIterator.mkString(\"|\", \"|\", \"|\")\n"+
      "}"
  }
  def dataRowClasses(n: Int) = {
    (1 to n).map { i =>  
        List(
          "case class "+dataRow(i)+parametersList(i)+" extends DataRow["+typesList(i, n)+"] {",
          if (i < n) "  def ![S"+(i+1)+"](t"+(i+1)+": S"+(i+1)+") = "+"DataRow"+(i+1)+parameters(i+1) else "",
          if (i < n) "  def !![S"+(i+1)+"](t"+(i+1)+": S"+(i+1)+") = "+"DataRow"+(i+1)+parameters(i+1) else "",
        "}").mkString("\n")
    }.mkString("\n")
  }
  def parametersList(i: Int) = (1 to i).map(j => "t"+j+": T"+j).mkString("(",", ", ")")
  def parameters(i: Int) = (1 to i).map("t"+_).mkString("(",", ", ")")
  def variantTypes(i: Int) = typesAsList(i).map("+"+_).mkString("[",", ", "]")
  def types(i: Int) = typesAsList(i).mkString("[",", ", "]")
  def typesAsList(i: Int): String =  (1 to i).map("T"+_)
  def typesList(i: Int): String = typesAsList(i).mkString(", ")
  def typesList(i: Int, n: Int): String =  (List(typesList(i)) ::: (i+1 to n).map(t => "Any").toList).mkString(", ")
  def typesTuple(i: Int) =  typesAsList(i).mkString("(",", ", ")")
  def dataRow(i: Int) = "DataRow"+i+variantTypes(i)
  def table(i: Int) = "Table"+i
}
*/
