package org.specs2
package form

import scala.xml._
import execute._
import StandardResults._
/**
 * A Cell is the Textual representation of a Form element.
 * 
 * It can be executed and converted to a String for display.
 */
trait Cell extends Text with Xml with Executable {
  def header = List(this)
  def setSuccess: Cell
  def setFailure: Cell
  def executeCell : Cell
}
/**
 * Base type for anything returning some text
 */
trait Text { 
  def text: String = padText(None)
  def padText(size: Option[Int]): String 
}
/**
 * Base type for anything returning some xml
 */
trait Xml {
  def xml: NodeSeq
  def colnumber: Int
}

/**
 * Simple Cell embedding an arbitrary String
 */
case class TextCell(s: String, result: Result = skipped) extends Cell {
  def padText(size: Option[Int]) = {
    size match {
      case None => s.toString
      case Some(n) => s.toString.padTo(n, ' ')
    }
  }
  def xml = <td class={execute.statusName}>{s}</td>
  def colnumber = 1
  def execute = result
  def setSuccess = TextCell(s, success)
  def setFailure = TextCell(s, failure)
  def executeCell = this
}
/**
 * Cell embedding a Field
 */
case class FieldCell(f: Field[_], result: Result = skipped) extends Cell {
  def padText(size: Option[Int]) = {
    size match {
      case None => f.toString
      case Some(s) => f.toString.padTo(s, ' ')
    }
  }
  def xml = (<td>{f.label}</td><td class="info">{f.get}</td>)
  def colnumber = 2

  def execute = result
  override def header = List(TextCell(f.label))
  def setSuccess = FieldCell(f, success)
  def setFailure = FieldCell(f, failure)
  def executeCell = this
}
/**
 * Cell embedding a Field
 */
case class PropCell(p: Prop[_,_], result: Option[Result] = None) extends Cell {
  def padText(size: Option[Int]) = {
    size match {
      case None => p.toString
      case Some(s) => p.toString.padTo(s, ' ')
    }
  }
  def xml = {
    val executed = result.getOrElse(skipped)
    <td>{p.label}</td><td class={executed.statusName}>{p.expected.map(_.toString).getOrElse("")}</td> ++
    (if (!executed.isSuccess) <td class={executed.statusName}>{executed.message}</td> else NodeSeq.Empty)
  }
  def colnumber = 2 + (if (result.map(_.isSuccess).getOrElse(false)) 2 else 0)

  def execute = result.getOrElse(p.execute)
  def executeCell = PropCell(p, result.orElse(Some(p.execute)))

  override def header = List(TextCell(p.label))
  def setSuccess = PropCell(p, Some(success))
  def setFailure = PropCell(p, Some(failure))
}
/**
 * Cell embedding a Form
 */
case class FormCell(form: Form) extends Cell {
  /** ignore the passed size and compute the max size on each row */
  def padText(size: Option[Int]): String = {
    form.allRows.map(_.padText(form.maxSizes)).mkString("\n")
  }
  def xml = form.toXml
  def colnumber = form.rows.map(_.cells.map(c => c.colnumber).max).max

  def execute = form.execute
  def executeCell = FormCell(form.executeForm)

  override def header = form.header
  def setSuccess = FormCell(form.setSuccess)
  def setFailure = FormCell(form.setFailure)
  
}

