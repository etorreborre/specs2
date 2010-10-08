package org.specs2
package form
import execute._
import matcher._
import scalaz._
import Scalaz._

case class Form(title: Option[String] = None, rows: List[Row] = (Nil: List[Row])) {
  def tr(c1: Cell, cs: Cell*) = {
    new Form(title, this.rows :+ Row.tr(c1, cs:_*))
  }
}
case class Row(cells: NonEmptyList[Cell]) {
  def text = cells.map(_.text).list.mkString("| ", " | ", " |")
}
case object Row {
  def tr(c1: Cell, cs: Cell*) = Row(nel1(c1, cs:_*))
}
case object Form {
  def apply(title: String) = new Form(Some(title), Nil)
  def tr(c1: Cell, c: Cell*) = new Form().tr(c1, c:_*)
}
trait Cell extends Text
case class DefaultCell(t: Any) extends Cell with Text {
  def text = t.toString
}
trait Text { def text: String }
case class FormText(form: Form) extends Cell with Text {
  def text: String = {
    (form.title.map("| "+_+" |").getOrElse("") :: form.rows.map(_.text)).
         filterNot(_.isEmpty).mkString("\n")
  }
}
object Forms {
  implicit def fieldsAreTextCell(t: Field[_]) = new DefaultCell(t)  
  implicit def propsAreCell(t: Prop[_, _]) = new DefaultCell(t)
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

}
