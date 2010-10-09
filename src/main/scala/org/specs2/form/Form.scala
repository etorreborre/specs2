package org.specs2
package form
import execute._
import matcher._
import scalaz.{ NonEmptyList, Scalaz }
import Scalaz.{ nel1 }
import specification._
import StandardResults._

case class Form(title: Option[String] = None, rows: List[Row] = (Nil: List[Row])) extends Executable {
  def tr(c1: Cell, cs: Cell*) = {
    new Form(title, this.rows :+ Row.tr(c1, cs:_*))
  }
  def execute = rows.foldLeft(success: Result) { (res, cur) => res and cur.execute }
}
case class Row(cells: NonEmptyList[Cell]) extends Executable {
  def text = cells.map(_.text).list.mkString("| ", " | ", " |")
  def execute = cells.list.foldLeft(success: Result) { (res, cur) => res and cur.execute }
}
case object Row {
  def tr(c1: Cell, cs: Cell*) = Row(nel1(c1, cs:_*))
}
case object Form {
  def apply(title: String) = new Form(Some(title), Nil)
  def tr(c1: Cell, c: Cell*) = new Form().tr(c1, c:_*)
}
trait Cell extends Text with Executable
case class FieldCell(f: Field[_]) extends Cell {
  def text = f.toString
  def execute = skipped
}
case class PropCell(p: Prop[_,_]) extends Cell {
  def text = p.toString
  def execute = p.execute
}
trait Text { def text: String }
case class FormText(form: Form) extends Cell {
  def text: String = {
    (form.title.map("| "+_+" |").getOrElse("") :: form.rows.map(_.text)).
         filterNot(_.isEmpty).mkString("\n")
  }
  def execute = form.execute
}
object Forms {
  implicit def fieldsAreTextCell(t: Field[_]) = new FieldCell(t)  
  implicit def propsAreCell(t: Prop[_, _]) = new PropCell(t)
  implicit def formsAreCell(t: Form) = new FormText(t)
  
  def form(title: String) = Form(title)

  def field[T](value: =>T): Field[T] = Field(value)
  def field[T](label: String, value: =>T): Field[T] = Field(label, value)
  def field(label: String, value1: Field[_], values: Field[_]*): Field[String] = Field(label, value1, values:_*)
  
  def prop(l: String) = Prop(l)
  def prop[T](label: String, actual: =>T) = Prop(label, actual)
  def prop[T, S](label: String, act: =>T, c: (T, S) => Result) = Prop(label, act, c)
  def prop[T, S](label: String, act: =>T, c: (S) => Matcher[T]) = Prop(label, act, c)
  def prop[T](value: =>T) = Prop(value)
  
  class FormFragment(form: Form) extends specification.Text(FormText(form).text)
  implicit def formsAreFragments(f: Form): Fragment = new FormFragment(f)
  implicit def formsAreExecutable(f: Form): Result = f.execute
}
