package org.specs2
package form

import scala.xml._
import xml.Nodex._
import text.NotNullStrings._
import main.Arguments
import execute._
import StandardResults._
import text.Markdown
import language.existentials
/**
 * A Cell is the Textual or Xml representation of a Form element: Field, Prop or Form.
 * A more general XmlCell is also available to be able to input any kind of Xml inside a Form
 * 
 * A Cell can be executed by executing the underlying element but also by setting the cell to a specific result (success or failure).
 * This feature is used to display rows of values with were expected and found ok in Forms.
 *
 */
trait Cell extends Text with Xml with Executable {
  def setSuccess: Cell = setResult(success)
  def setFailure: Cell = setResult(failure)
  def setSkipped: Cell = setResult(skipped)
  def setPending: Cell = setResult(pending)
  def setResult(r: Result): Cell

  /**
   * execute the Cell and returns it
   */
  def executeCell : Cell
}
/**
 * Base type for anything returning some text
 */
trait Text {
  /** @return a text representation */
  def text: String

  /** @return the width of the cell, without borders when it's a FormCell */
  def width: Int = text.size
}
/**
 * Base type for anything returning some xml
 */
trait Xml {
  /** @return an xml representation */
  def xml(implicit args: Arguments): NodeSeq
}

/**
 * utility functions for creating xml for Cells
 */
object Xml {
  /** @return the stacktraces of a Cell depending on its type and execution result */
  def stacktraces(cell: Cell)(implicit args: Arguments): NodeSeq = cell match {
    case FormCell(f: Form)                           => f.rows.map(stacktraces(_)).reduceNodes
    case PropCell(_, Some(e @ Error(_, _)))          => stacktraces(e)
    case PropCell(_, Some(f @ Failure(_, _, _, _)))  => stacktraces(f)
    case FieldCell(_, Some(e @ Error(_, _)))         => stacktraces(e)
    case other                                       => NodeSeq.Empty
  }

  private def stacktraces(row: Row)(implicit args: Arguments): NodeSeq = row.cells.map(stacktraces(_)).reduceNodes

  private def stacktraces(e: Result with ResultStackTrace): NodeSeq =
    <div class="formstacktrace details" id={System.identityHashCode(e).toString}>
      {e.message.notNull+" ("+e.location+")"}
      {e.stackTrace.map(st => <div>{st}</div>)}
    </div>

  /** @return  the number of columns for a given cell */
  def colnumber(cell: Cell): Int = cell match {
    case TextCell(_,_,_)  => 1 // just the string
    case FieldCell(_, _)  => 3 // label + value + optional error
    case PropCell(_, _)   => 3 // label + value + optional error/failure
    case EffectCell(_, _) => 2 // label + optional error
    case FormCell(form)   => if (form.rows.isEmpty) 1 else form.rows.map(_.cells.map(c => colnumber(c)).sum).max
    case LazyCell(c)      => colnumber(c)
    case _                => 100 // not known by default, so a max value is chosen
  }
}

/**
 * Simple Cell embedding an arbitrary String
 */
case class TextCell(s: String, result: Option[Result] = None, decorator: Decorator = Decorator()) extends Cell with DecoratedProperty[TextCell] {

  def text = s

  def xml(implicit args: Arguments) = <td class={result.map(_.statusName).getOrElse("none")} style="info">{decorateValue(Markdown.toXhtml(text))}</td>

  def execute = result.getOrElse(Skipped())
  def setResult(r: Result) = TextCell(s, result)
  def executeCell = this

  /** set a new Decorator */
  def decoratorIs(d: Decorator) = copy(decorator = d)

  override def equals(other: Any) = {
    other match {
      case TextCell(s1, result1, _) => s == s1 && result == result1
      case _                        => false
    }
  }

}

object TextCell {
  def apply(s: String, result: Result): TextCell = new TextCell(s, Some(result))
}
/**
 * Cell embedding a Field
 */
case class FieldCell(f: Field[_], result: Option[Result] = None) extends Cell {
  def text = f.toString

  def xml(implicit args: Arguments) = {
    val executedValue = f.valueOrResult match {
      case Left(e)  => e
      case Right(e) => e
    }
    val executedResult = execute
    (<td style={f.labelStyles}>{f.decorateLabel(f.label)}</td> unless f.label.isEmpty) ++
     <td class={statusName(executedResult)} style={f.valueStyles}>{f.decorateValue(executedValue)}</td> ++
    (<td class={executedResult.statusName} onclick={"showHide("+System.identityHashCode(executedResult).toString+")"}>{executedResult.message}</td> unless
      !executedResult.isError)
  }

  private def statusName(r: Result) = r match {
    case Skipped(_, _) => "info"
    case _             => r.statusName
  }

  def execute = result.getOrElse(f.execute)
  def setResult(r: Result) = FieldCell(f, Some(r))
  def executeCell = FieldCell(f, result.orElse(Some(f.execute)))

}
/**
 * Cell embedding a Eff
 */
case class EffectCell(e: Effect[_], result: Option[Result] = None) extends Cell {
  def text = e.toString

  def xml(implicit args: Arguments) = {
    val executed = e.valueOrResult match {
      case Left(r)  => r
      case Right(r) => r
    }
    val executedResult = execute
    <td style={e.labelStyles} class="info">{e.decorateLabel(e.label)}</td> ++
    (<td class={executedResult.statusName} onclick={"showHide("+System.identityHashCode(executedResult).toString+")"}>{executedResult.message}</td> unless executedResult.isSuccess)
  }

  private def statusName(r: Result) = r match {
    case Skipped(_, _) => "info"
    case Success(_, _) => "info"
    case _             => r.statusName
  }

  def execute = result.getOrElse(e.execute)
  def setResult(r: Result) = EffectCell(e, Some(r))
  def executeCell = EffectCell(e, result.orElse(Some(e.execute)))

}

/**
 * Cell embedding a Prop
 */
case class PropCell(p: Prop[_,_], result: Option[Result] = None) extends Cell {
  def text = p.toString

  def execute = result.getOrElse(p.execute)
  def executeCell = PropCell(p, result.orElse(Some(p.execute)))
  def setResult(r: Result) = PropCell(p, Some(r))

  def xml(implicit args: Arguments): NodeSeq = {
    val executed = result.getOrElse(skipped)
    (<td style={p.labelStyles}>{p.decorateLabel(p.label)}</td> unless p.label.isEmpty) ++
    (<td class={executed.statusName}>{p.decorateValue(p.expectedValue.right.toOption.getOrElse(""))}</td> unless !p.expectedValue.right.toOption.isDefined) ++
    (<td class={executed.statusName} onclick={"showHide("+System.identityHashCode(executed).toString+")"}>{executed.message}</td> unless (executed.isSuccess || executed.message.isEmpty))
  }
}
/**
 * Cell embedding a Form
 */
class FormCell(_form: =>Form, result: Option[Result] = None) extends Cell {
  lazy val form = _form

  def text: String = form.text

  def xml(implicit args: Arguments) = form.toCellXml(args)

  def execute = result.getOrElse(form.execute)
  def executeCell = {
    lazy val executed = result.map(r => form).getOrElse(form.executeForm)
    new FormCell(executed, result.orElse(Some(executed.execute)))
  }
  def setResult(r: Result) = new FormCell(form.setResult(r), Some(r))

  /**
   * @return the width of a form when inlined.
   *         It is the width of its text size minus 4, which is the size of the borders "| " and " |"
   */
  override def width = text.split("\n").map((_:String).size).max[Int] - 4
}
object FormCell {
  def unapply(cell: FormCell): Option[Form] = Some(cell.form)
}
/** Proxy to a cell that's not evaluated right away when added to a row */
class LazyCell(_cell: =>Cell) extends Cell {
  lazy val cell = _cell
  def text: String = cell.text
  def xml(implicit args: Arguments) = cell.xml(args)
  def execute = cell.execute
  def executeCell = cell.executeCell
  def setResult(r: Result) = cell.setResult(r)
}
object LazyCell {
  def unapply(cell: LazyCell): Option[Cell] = Some(cell.cell)
}
/** This cell can contain any xml */
class XmlCell(_theXml: =>NodeSeq) extends Cell {
  lazy val theXml = _theXml
  def text: String = theXml.text
  def xml(implicit args: Arguments) = theXml
  def execute = success
  def executeCell = this
  def setResult(r: Result) = this
}
object XmlCell {
  def unapply(cell: XmlCell): Option[NodeSeq] = Some(cell.theXml)
}

