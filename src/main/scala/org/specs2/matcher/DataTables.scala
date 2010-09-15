package org.specs2
package matcher
import execute._

trait DataTables {
  implicit def toTableHeader(a: String) = new TableHeader(List(a))
  implicit def toDataRow[T](a: T) = DataRow1(a)
  case class TableHeader(titles: List[String]) {
	def |(title: String) = copy(titles = this.titles :+ title)
	def |[T1, T2, T3](row: DataRow3[T1, T2, T3]) = new Table3(titles, List(row))
	def |>[T1, T2, T3](row: DataRow3[T1, T2, T3]) = new Table3(titles, List(row), execute = true)
  }
  class Table(val titles: List[String], val execute: Boolean = false) {
	def showTitles = titles.mkString("|", "|", "|")
	def collect[R <% Result](results: List[(String, R)]): Result = {
	  val totalSuccess = results.foldLeft(Success(""): Result)((res, cur) => res and cur._2)
	  val collected = 
	  results.map { (cur: (String, R)) => result(cur._1, cur._2) }.mkString("\n")
	  val header = totalSuccess match {
	 	case Success(_) => showTitles
	 	case other      => "  " + showTitles  
	  }
	  totalSuccess.copyMessage(header+"\n"+collected)
	}
	private def result(desc: String, result: Result): String = {
	  result.status+" "+desc+{
	 	 result match {
	 	   case Success(_) => ""
	 	   case _ => " " + result.message
	 	 }
	  }
	}
  }
  case class Table3[T1, T2, T3](override val titles: List[String], rows: List[DataRow3[T1, T2, T3]], override val execute: Boolean = false) extends 
  Table(titles, execute) { outer =>
	def |(row: DataRow3[T1, T2, T3]) = Table3(titles, outer.rows :+ row, execute)
	def |[R <% Result](f: (T1, T2, T3) => R) = executeRow(f, execute)
	def |>[R <% Result](f: (T1, T2, T3) => R) = executeRow(f, true)
	def executeRow[R <% Result](f: (T1, T2, T3) => R, exec: Boolean): Result = { 
	  if (exec) {
	    collect(rows map { (d: DataRow3[T1, T2, T3]) => 
	      (d.show, f(d.t1, d.t2, d.t3)) 
	    })
	  }
	  else Success("ok")
	}
  }
  abstract class DataRow[T1, T2, T3] extends Product {
	def show = productIterator.mkString("|", "|", "|")
  }
  case class DataRow1[T1](t1: T1) extends DataRow[T1, Any, Any] {
	def ![S2](t2: S2) = {
	  DataRow2(t1, t2)
	}
  }	
  case class DataRow2[T1, T2](t1: T1, t2: T2) extends DataRow[T1, T2, Any] {
	def ![S3](t3: S3) = {
	  DataRow3(t1, t2, t3)
	}
  }	
  case class DataRow3[T1, T2, T3](val t1: T1, t2: T2, t3: T3) extends DataRow[T1, T2, T3] {
  }
}
private object DataTablesGenerator {
  def main(args: Array[String]) = {
	println(all(3))  
  }
  def all(n: Int) = {
	List(rows(n), tableClasses(n), dataRowClass(n), dataRowClasses(n)).mkString("\n")
  }
  def parametersList(i: Int) = (1 to i).map(j => "t"+j+": T"+j).mkString("(",", ", ")")
  def parameters(i: Int) = (1 to i).map("t"+_).mkString("(",", ", ")")
  def types(i: Int) = (1 to i).map("T"+_).mkString("[",", ", "]")
  def typesList(i: Int): String =  (1 to i).map("T"+_).mkString(", ")
  def typesList(i: Int, n: Int): String =  List(typesList(i), (i to n-i).map("Any").mkString(",")).mkString(",")
  def typesTuple(i: Int) =  (1 to i).map("T"+_).mkString("(",", ", ")")
  def dataRow(i: Int) = "DataRow"+i+types(i)
  def table(i: Int) = "Table"+i
  def rows(n: Int) = {
    (1 to n).flatMap { i =>
      val addRow = types(i)+"(row: "+dataRow(i)+") = new "+table(i)+"(titles, List(row)"
      val addRowStill = "def |"+addRow 
      val addRowExecute = "def |>"+addRow 
	  List(addRowStill, addRowExecute+", execute = true").map(_+")")
    }.mkString("\n")
  }
  
  def tableClasses(n: Int) = {
	(1 to n).map { i =>  
	  List("case class Table"+i+types(i)+"(override val titles: List[String], rows: List["+dataRow(i)+"], override val execute: Boolean = false) extends"+ 
           "Table(titles, execute) { outer =>",
	       "  def |(row: "+dataRow(i)+") = "+table(i)+"(titles, outer.rows :+ row, execute)",
	       "  def |[R <% Result](f: "+typesTuple(i)+" => R) = executeRow(f, execute)",
	       "  def |>[R <% Result](f: "+typesTuple(i)+" => R) = executeRow(f, true)",
	       "  def executeRow[R <% Result](f: "+typesTuple(i)+" => R, exec: Boolean): Result = {", 
	       "    if (exec) {",
	       "      collect(rows map { (d: "+dataRow(i)+") =>", 
	       "        (d.show, f("+(1 to i).map("d.t"+_).mkString(",")+"))",
	       "    })",
	       "  }",
	       "  else Success(\"ok\")",
	       "}").mkString("\n") 		
	}
  }
  def dataRowClass(n: Int) = {
	"abstract class "+dataRow(n)+" extends Product {\n"+
	"  def show = productIterator.mkString(\"|\", \"|\", \"|\")\n"+
    "}"
  }
  def dataRowClasses(n: Int) = {
	(1 to n).map { i =>  
      List(
      	"case class "+dataRow(i)+parametersList(i)+" extends DataRow["+typesList(i, n)+"] {",
        " def !"+types(i+1)+"(t"+(i+1)+": S"+(i+1)+") = "+"DataRow"+i+parameters(i+1),
	    "}").mkString("\n")
	}
  }
}
