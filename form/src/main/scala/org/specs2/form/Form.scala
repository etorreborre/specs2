package org.specs2.form

import scala.xml.{Text as _,*}
import org.specs2.collection.Seqx.*
import org.specs2.xml.Nodex.{given, *}
import org.specs2.execute.*
import org.specs2.main.Arguments
import StandardResults.*
import org.specs2.matcher.*
import DecoratedProperties.*
import ResultLogicalCombinators.*
import org.specs2.control.Properties.{given, *}
import org.specs2.control.ImplicitParameters.*
import org.specs2.control.Use
import reflect.Selectable.reflectiveSelectable

/**
 * A Form is a container for Rows (@see Row) where each row contain some Cell (@see Cell).
 * It has an optional title and possibly no rows.
 *
 * A Form can be executed by executing each row and collecting the results.
 */
class Form(val title: Option[String] = None, val rows: Seq[Row] = Vector(),  val result: Option[Result] = None) extends Executable with Text:

  /** @return all rows, including the header */
  lazy val allRows = title.map(t => Row.tr(TextCell(t))).toSeq ++ rows

  /** @return the maximum cell size, column by column */
  lazy val maxSizes = allRows.map(_.cells).safeTranspose.map(l => l.map(_.width).max[Int])

  /** @return a new Form. This method can be overridden to return a more accurate subtype */
  protected def newForm(title: Option[String] = None, rows: Seq[Row] = Vector(), result: Option[Result] = None): Form =
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
  def th(hs: Seq[Field[?]]): Form = tr(Row.tr(hs.map((f: Field[?]) => FieldCell(f.header))))
  /** add a new Header, with at least one Field */
  def th(h1: Field[?], hs: Field[?]*): Form = tr(Row.tr(FieldCell(h1.header), hs.map((f: Field[?]) => FieldCell(f.header))*))
  /** add a new Header */
  def th(hs: Seq[String])(using p: ImplicitParam): Form = Use.ignoring(p)(th(hs.map(Field(_))))
  /** add a new Header, with at least one Field */
  def th(h1: String, hs: String*): Form = th(Field(h1), hs.map(Field(_))*)
  /** add a new Row, with at least one Cell */
  def tr(c1: Cell, cs: Cell*): Form = tr(Row.tr(c1, cs*))
  /** add a new Row */
  def tr(row: Row): Form = newForm(title, this.rows :+ row, result)
  /** create new tabs in the Form */
  def tabs[T](values: Seq[T])(f: T => Tabs) = tr(values.foldLeft(Tabs()) { (res, cur) => res.tabs(f(cur)) })
  /** create new rows in the Form */
  def trs[T](values: Seq[T])(f: T => Row) = values.foldLeft(this) { (res, cur) => res.tr(f(cur)) }

  /** add the rows of a form */
  private def addRows(f: Form): Form =
    val oldRowsAndTitle = f.title.map(th(_)).getOrElse(this).rows
    newForm(title, oldRowsAndTitle ++ f.rows, result)

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
  def executeForm: Form =
    if result.isDefined then
      this
    else
      val executedRows = executeRows
      newForm(title, executedRows, Some(executedRows.map(_.execute).foldLeft(success: Result) { (res, cur) => res and cur }))

  /** @return the printed form with a padding space size to use for each cell */
  def text: String = allRows.map(_.text(maxSizes)).mkString("\n")

  /** @return an xml description of this form */
  def toXml(using args: Arguments = Arguments()): NodeSeq =
    Form.toXml(this)

  /** @return an xml description of this form, to be embedded in a Cell */
  def toCellXml(using args: Arguments = Arguments()): NodeSeq =
    <td class="info">{Form.toXml(this)(using args)}</td>

  def subset(f1: Traversable[Form], f2: Traversable[Form]): Form =
    addLines(FormDiffs.subset(f1.toSeq, f2.toSeq))

  def subsequence(f1: Traversable[Form], f2: Traversable[Form]): Form =
    addLines(FormDiffs.subsequence(f1.toSeq, f2.toSeq))

  def set(f1: Traversable[Form], f2: Traversable[Form]): Form =
    addLines(FormDiffs.set(f1.toSeq, f2.toSeq))

  def sequence(f1: Traversable[Form], f2: Traversable[Form]): Form =
    addLines(FormDiffs.sequence(f1.toSeq, f2.toSeq))

  def subset[T1 : HasForm, T2 : HasForm](f1: Seq[T1], f2: Seq[T2]): Form =
    addLines(FormDiffs.subset(f1.map(_.form), f2.map(_.form)))

  def subsequence[T : HasForm](f1: Seq[T], f2: Seq[T]): Form =
    addLines(FormDiffs.subsequence(f1.map(_.form), f2.map(_.form)))

  def set[T : HasForm](f1: Seq[T], f2: Seq[T]): Form =
    addLines(FormDiffs.set(f1.map(_.form), f2.map(_.form)))

  def sequence[T : HasForm](f1: Seq[T], f2: Seq[T]): Form =
    addLines(FormDiffs.sequence(f1.map(_.form), f2.map(_.form)))

  /**
   * encapsulate this form into an effect
   */
  def toEffect(label: String) = Effect(label, executeForm)

  /**
   * encapsulate this form into a Prop
   */
  def toProp(label: String): Prop[Form, Any] =
    lazy val executed = executeForm
    lazy val executedResult = executed.execute match
      case s @ Success(_,_) => s
      case Failure(_,_,_,_) => Failure("failed")
      case Error(_,_)       => Error("error")
      case other            => other

    Prop(label, executed, (f: Form, s: Any) => executedResult).apply {
      if executedResult.isSuccess then
        "success": Any
      else
        Form.toXml(executed)(using Arguments())
    }

  /**
   * transform this form to a form that will be added as a <td> element inside another form
   */
  def inline: InlinedForm = new InlinedForm(title, rows, result)

  private def addLines(fs: Seq[Form]) = fs.foldLeft(this) { (res, cur) =>  res.addRows(cur) }

  override def equals(a: Any) = a.asInstanceOf[Matchable] match
    case f: Form => f.title == title && rows == f.rows
    case _       => false

  override def hashCode =
    title.hashCode

/**
 *  Companion object of a Form to create:
 *   - an empty Form
 *   - a Form with no rows but a title
 *   - a Form with no title but one row
 *
 */
case object Form:
  /** @return an empty form */
  def apply(): Form = new Form()
  /** @return an empty form with a title */
  def apply(title: String): Form = new Form(Some(title))
  /** create a Form from a DataTable */
  def apply(table: DataTable): Form =
    def firstField[A](as: Seq[A]) = Field(as.headOption.getOrElse(""))
    def otherFields[A](as: Seq[A]) = as.drop(1).map(Field(_))

    val headerRest = otherFields(table.titles) ++ (if table.isSuccess then Seq[Field[?]]() else Seq(Field("message")))
    table.rows.foldLeft(th(firstField(table.titles), headerRest*)) { (res, cur) =>
      val values = Row.tr(FieldCell(firstField(cur.cells)), otherFields(cur.cells).map(FieldCell(_))*)
      res.tr {
        if cur.result.isSuccess then      values
        else if cur.result.isFailure then values.add(FieldCell(Field(cur.result.message)).setResult(cur.result))
        else                           values.add(FieldCell(Field("error").bold).setResult(cur.result))
      }
    }
  /** @return a Form with one row */
  def tr(c1: Cell, cs: Cell*): Form = new Form().tr(c1, cs*)
  /** @return a Form with one row */
  def tr(row: Row): Form = new Form().tr(row)
  /** @return a Form with one row and cells formatted as header cells */
  def th(h1: Field[?], hs: Field[?]*): Form = new Form().th(h1, hs*)
  /** @return a Form with one row and cells formatted as header cells */
  def th(h1: String, hs: String*): Form = new Form().th(h1, hs*)
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
  def toXml(form: Form)(using args: Arguments) =
    <form>
    <table>{titleAndRows(form)}</table>{formStacktraces(form)}</form>
  /**
   * This method creates an xml representation of a Form as an Html table rows,
   * ready to be embedded in a table
   *
   * @return the xml representation of a Form
   */
  def titleAndRows(form: Form)(using args: Arguments = Arguments()) =
    val colnumber = Xml.colnumber(new FormCell(form))
    title(form, colnumber) ++
    rows(form, colnumber)
  /**
   * This method creates a div to display the exceptions of a form
   * ready to be embedded in a table
   *
   */
  def formStacktraces(form: Form)(using args: Arguments = Arguments()) =
    val traces = Xml.stacktraces(new FormCell(form))
    if traces.isEmpty then NodeSeq.Empty
    else <pre><i>[click on failed cells to see the stacktraces]</i>{traces}</pre>

  /**
   * Private methods for building the Form xml
   */
  private def title(form: Form, colnumber: Int) = form.title.map(t => <tr><th colspan={(colnumber+1).toString}>{t}</th></tr>).toList.reduceNodes
  private def rows(form: Form, colnumber: Int)(using args: Arguments) = form.rows.map(row(_, colnumber)).reduceNodes
  private def row(r: Row, colnumber: Int)(using args: Arguments) =
    val spanned = r.cells.dropRight(1).map(cell(_)) ++ cell(r.cells.last, colnumber - r.cells.size + 1)
    <tr>{spanned}</tr>

  private def cell(c: Cell, colnumber: Int = 0)(using args: Arguments) =
    if colnumber > 1 then
      c.xml(using args).toList match
        case start :+ (e: Elem) => start ++ (e % ("colspan" -> colnumber.toString))
        case other                         => other
    else
      c.xml(using args).toList

  /** a Form can be implicitly transformed to results */
  given AsResult[Form] with
    def asResult(f: =>Form): Result =
      f.execute

trait HasForm[T]:
  def getForm(t: T): Form

  extension (t: T)
    def form: Form =
      getForm(t)

given HasForm[Form] with
  def getForm(f: Form): Form =
    f

given [T <: {def form: Form}]: HasForm[T] with
  def getForm(t: T): Form =
    t.form
