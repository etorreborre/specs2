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
	 	   case Success(_) =>
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