package org.specs2
package form

import scala.xml.{ Text => _, _ }
import collection.Seqx._
import xml.Nodex._
import execute._
import main.Arguments
import StandardResults._
import matcher._
import DecoratedProperties._
import ResultLogicalCombinators._
import control.ImplicitParameters._
import control.Use

/**
 * A Form is a container for Rows (@see Row) where each row contain some Cell (@see Cell).
 * It has an optional title and possibly no rows.
 * 
 * A Form can be executed by executing each row and collecting the results.
 */
class Form(val title: Option[String] = None, val rows: Seq[Row] = Vector(),  val result: Option[Result] = None) extends Executable with Text {

  /** @return all rows, including the header */
  lazy val allRows = title.map(t => Row.tr(TextCell(t))).toSeq ++ rows

  /** @return the maximum cell size, column by column */
  lazy val maxSizes = allRows.map(_.cells).safeTranspose.map(l => l.map(_.width).max[Int])

  /** @return a new Form. This method can be overridden to return a more accurate subtype */
  protected def newForm(title: Option[String] = None, rows: Seq[Row] = Vector(), result: Option[Result] = None) =
    new Form(title, rows, result)
  /** @return a Form where every Row is executed with a Success */
  def setSuccess = setResult(success)
  /** @return a Form where every Row is executed with a Failure */
  def setFailure = setResult(failure)
  /** @return a Form where every Row is executed with a Skipped */
  def setSkipped = setResult(skipped)
  /** @return a Form where every Row is executed with a Pending */
  def setPending = setResult(pending)
  /** @return a Form where every Row is executed with a given Result */
  def setResult(r: Result) = newForm(title, rows.map(_.setResult(r)), Some(r))


  /** add a new Header with some fields */
  def th(hs: Seq[Field[_]]): Form = tr(Row.tr(hs.map((f: Field[_]) => FieldCell(f.header))))
  /** add a new Header, with at least one Field */
  def th(h1: Field[_], hs: Field[_]*): Form = tr(Row.tr(FieldCell(h1.header), hs.map((f: Field[_]) => FieldCell(f.header)):_*))
  /** add a new Header */
  def th(hs: Seq[String])(implicit p: ImplicitParam): Form = Use.ignoring(p)(th(hs.map(Field(_))))
  /** add a new Header, with at least one Field */
  def th(h1: String, hs: String*): Form = th(Field(h1), hs.map(Field(_)):_*)
  /** add a new Row, with at least one Cell */
  def tr(c1: Cell, cs: Cell*): Form = tr(Row.tr(c1, cs:_*))
  /** add a new Row */
  def tr(row: Row): Form = newForm(title, this.rows :+ row, result)
  /** create new tabs in the Form */
  def tabs[T](values: Seq[T])(f: T => Tabs) = tr(values.foldLeft(Tabs()) { (res, cur) => res.tabs(f(cur)) })
  /** create new rows in the Form */
  def trs[T](values: Seq[T])(f: T => Row) = values.foldLeft(this) { (res, cur) => res.tr(f(cur)) }

  /** add the rows of a form */
  private def addRows(f: Form): Form = {
    val oldRowsAndTitle = f.title.map(th(_)).getOrElse(this).rows
    newForm(title, oldRowsAndTitle ++ f.rows, result)
  }

  /**
   * execute all rows
   * @return a logical and on all results 
   */
  def execute = result getOrElse (executeForm.result getOrElse success)
  def executeRows = rows.map(_.executeRow)
  /**
   * execute all rows
   * @return an executed Form
   */
  def executeForm = {
    if (result.isDefined) this
    else {
      val executedRows = executeRows
      newForm(title, executedRows, Some(executedRows.map(_.execute).foldLeft(success: Result) { (res, cur) => res and cur }))
    }
  }

  /** @return the printed form with a padding space size to use for each cell */
  def text: String = allRows.map(_.text(maxSizes)).mkString("\n")

  /** @return an xml description of this form */
  def toXml(implicit args: Arguments = Arguments()) = Form.toXml(this)(args)
  /** @return an xml description of this form, to be embedded in a Cell */
  def toCellXml(implicit args: Arguments = Arguments()) = <td class="info">{Form.toXml(this)(args)}</td>

  def subset(f1: Traversable[Form], f2: Traversable[Form]): Form = {
    addLines(FormDiffs.subset(f1.toSeq, f2.toSeq))
  }
  def subsequence(f1: Traversable[Form], f2: Traversable[Form]): Form = {
    addLines(FormDiffs.subsequence(f1.toSeq, f2.toSeq))
  }
  def set(f1: Traversable[Form], f2: Traversable[Form]): Form = {
    addLines(FormDiffs.set(f1.toSeq, f2.toSeq))
  }
  def sequence(f1: Traversable[Form], f2: Traversable[Form]): Form = {
    addLines(FormDiffs.sequence(f1.toSeq, f2.toSeq))
  }
  def subset[T <: Any { def form: Form }](f1: Seq[T], f2: Seq[T]): Form = {
    addLines(FormDiffs.subset(f1.map(_.form), f2.map(_.form)))
  }
  def subsequence[T <: Any { def form: Form }](f1: Seq[T], f2: Seq[T]): Form = {
    addLines(FormDiffs.subsequence(f1.map(_.form), f2.map(_.form)))
  }
  def set[T <: Any { def form: Form }](f1: Seq[T], f2: Seq[T]): Form = {
    addLines(FormDiffs.set(f1.map(_.form), f2.map(_.form)))
  }
  def sequence[T <: Any { def form: Form }](f1: Seq[T], f2: Seq[T]): Form = {
    addLines(FormDiffs.sequence(f1.map(_.form), f2.map(_.form)))
  }

  /**
   * encapsulate this form into an effect
   */
  def toEffect(label: String) = Effect(label, executeForm)

  /**
   * encapsulate this form into a Prop
   */
  def toProp(label: String) = {
    lazy val executed = executeForm
    lazy val executedResult = executed.execute match {
      case s @ Success(_,_) => s
      case Failure(_,_,_,_) => Failure("failed")
      case Error(_,_)       => Error("error")
      case other            => other
    }
    Prop[Form, Any](label, executed, (f: Form, s: Any) => executedResult) {
      if (executedResult.isSuccess)
        "success"
      else
        Form.toXml(executed)(Arguments())
    }
  }

  /**
   * transform this form to a form that will be added as a <td> element inside another form
   */
  def inline: InlinedForm = new InlinedForm(title, rows, result)

  private def addLines(fs: Seq[Form]) = fs.foldLeft(this) { (res, cur) =>  res.addRows(cur) }

  override def equals(a: Any) = a match {
    case f: Form => f.title == title && rows == f.rows
    case _       => false
  }

  override def hashCode =
    title.hashCode
}

/**
 *  Companion object of a Form to create:
 *   - an empty Form
 *   - a Form with no rows but a title
 *   - a Form with no title but one row
 *
 */
case object Form {
  /** @return an empty form */
  def apply(): Form = new Form()
  /** @return an empty form with a title */
  def apply(title: String): Form = new Form(Some(title))
  /** create a Form from a DataTable */
  def apply(table: DataTable): Form = {
    def firstField[A](as: Seq[A]) = Field(as.headOption.getOrElse(""))
    def otherFields[A](as: Seq[A]) = as.drop(1).map(Field(_))

    val headerRest = otherFields(table.titles) ++ (if (table.isSuccess) Seq[Field[_]]() else Seq(Field("message")))
    table.rows.foldLeft(th(firstField(table.titles), headerRest:_*)) { (res, cur) =>
      val values = Row.tr(FieldCell(firstField(cur.cells)), otherFields(cur.cells).map(FieldCell(_)):_*)
      res.tr {
        if (cur.result.isSuccess)      values
        else if (cur.result.isFailure) values.add(FieldCell(Field(cur.result.message)).setResult(cur.result))
        else                           values.add(FieldCell(Field("error").bold).setResult(cur.result))
      }
    }
  }
  /** @return a Form with one row */
  def tr(c1: Cell, cs: Cell*): Form = new Form().tr(c1, cs:_*)
  /** @return a Form with one row */
  def tr(row: Row): Form = new Form().tr(row)
  /** @return a Form with one row and cells formatted as header cells */
  def th(h1: Field[_], hs: Field[_]*): Form = new Form().th(h1, hs:_*)
  /** @return a Form with one row and cells formatted as header cells */
  def th(h1: String, hs: String*): Form = new Form().th(h1, hs:_*)
  /** create new tabs in the Form */
  def tabs[T](values: Seq[T])(f: T => Tabs) = new Form().tabs(values)(f)
  /** create new rows in the Form */
  def trs[T](values: Seq[T])(f: T => Row) = new Form().trs(values)(f)

  /**
   * This method creates an xml representation of a Form as an Html table
   *
   * If the Form has issues, stacktraces are written and hidden under the table
   *
   * @return the xml representation of a Form
   */
  def toXml(form: Form)(implicit args: Arguments) = {
    <form>
    <table>{titleAndRows(form)}</table>{formStacktraces(form)}</form>
  }
  /**
   * This method creates an xml representation of a Form as an Html table rows,
   * ready to be embedded in a table
   *
   * @return the xml representation of a Form
   */
  def titleAndRows(form: Form)(implicit args: Arguments = Arguments()) = {
    val colnumber = Xml.colnumber(new FormCell(form))
    title(form, colnumber) ++
    rows(form, colnumber)
  }
  /**
   * This method creates a div to display the exceptions of a form
   * ready to be embedded in a table
   *
   */
  def formStacktraces(form: Form)(implicit args: Arguments = Arguments()) = {
    val traces = Xml.stacktraces(new FormCell(form))
    if (traces.isEmpty) NodeSeq.Empty
    else <pre><i>[click on failed cells to see the stacktraces]</i>{traces}</pre>
  }

  /**
   * Private methods for building the Form xml
   */
  private def title(form: Form, colnumber: Int) = form.title.map(t => <tr><th colspan={(colnumber+1).toString}>{t}</th></tr>).toList.reduceNodes
  private def rows(form: Form, colnumber: Int)(implicit args: Arguments) = form.rows.map(row(_, colnumber)).reduceNodes
  private def row(r: Row, colnumber: Int)(implicit args: Arguments) = {
    val spanned = r.cells.dropRight(1).map(cell(_)) ++ cell(r.cells.last, colnumber - r.cells.size + 1)
    <tr>{spanned}</tr>
  }

  private def cell(c: Cell, colnumber: Int = 0)(implicit args: Arguments) = {
    if (colnumber > 1) {
      c.xml(args).toList match {
      case start :+ (e: Elem) => start ++ (e % ("colspan" -> colnumber.toString))
        case other                         => other
      }
    } else
      c.xml(args).toList
  }

  /** a Form can be implicitly transformed to results */
  implicit def formAsResult: AsResult[Form] = new AsResult[Form] {
    def asResult(f: =>Form): Result = f.execute
  }

}

