package org.specs2
package form

import scala.xml._
import xml.Nodex._
import text.NotNullStrings._
import main.Arguments
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
  def xml(implicit args: Arguments): NodeSeq
  def stacktraces(implicit args: Arguments): NodeSeq
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
  def xml(implicit args: Arguments) = <td class={execute.statusName}>{s}</td>
  def stacktraces(implicit args: Arguments) = NodeSeq.Empty

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
  def xml(implicit args: Arguments) =
    (<td style={f.labelStyles}>{f.decorateLabel(f.label)}</td> unless f.label.isEmpty) ++
    <td class={statusName(result)} style={f.valueStyles}>{f.decorateValue(f.get)}</td>

  private def statusName(r: Result) = r match {
    case Skipped(_, _) => "info"
    case _             => r.statusName
  }

  def stacktraces(implicit args: Arguments) = NodeSeq.Empty

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

  def colnumber = 2 + (if (result.map(_.isSuccess).getOrElse(false)) 2 else 0)

  def execute = result.getOrElse(p.execute)
  def executeCell = PropCell(p, result.orElse(Some(p.execute)))
  override def header = List(TextCell(p.label))
  def setSuccess = PropCell(p, Some(success))
  def setFailure = PropCell(p, Some(failure))

  def xml(implicit args: Arguments): NodeSeq = {
    val executed = result.getOrElse(skipped)
    (<td>{p.decorateLabel(p.label)}</td> unless p.label.isEmpty) ++
     <td class={executed.statusName}>{p.decorateValue(p.expected.map(_.toString).getOrElse(""))}</td> ++
    (<td class={executed.statusName} onclick={"showHide("+System.identityHashCode(executed).toString+")"}>{executed.message}</td> unless executed.isSuccess)
  }

  def stacktraces(implicit args: Arguments): NodeSeq = result match {
    case Some(e @ Error(_, _))                           => stacktraces(e)
    case Some(f @ Failure(_, _, _, _)) if args.failtrace => stacktraces(f)
    case _                                               => NodeSeq.Empty
  }

  private def stacktraces(e: Result with ResultStackTrace): NodeSeq =
    <div class="formstacktrace details" id={System.identityHashCode(e).toString}>
      {e.message.notNull+" ("+e.location+")"}
      {e.stackTrace.map(st => <div>{st}</div>)}
    </div>
}
/**
 * Cell embedding a Form
 */
class FormCell(form: =>Form) extends Cell {
  /** ignore the passed size and compute the max size on each row */
  def padText(size: Option[Int]): String = {
    form.allRows.map(_.padText(form.maxSizes)).mkString("\n")
  }
  def xml(implicit args: Arguments) = form.toXml(args)
  def colnumber = if (form.rows.isEmpty) 1 else form.rows.map(_.cells.map(c => c.colnumber).sum).max

  def execute = form.execute
  def executeCell = new FormCell(form.executeForm)

  override def header = form.header
  def setSuccess = new FormCell(form.setSuccess)
  def setFailure = new FormCell(form.setFailure)
  def stacktraces(implicit args: Arguments) = Form.stacktraces(form)(args)
}

