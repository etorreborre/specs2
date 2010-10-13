package org.specs2
package form
import execute._
import matcher._
import specification._

/**
 * Utility methods to build Fields, Props and Forms to insert in other Forms or Fragments.
 */
trait Forms extends FormFragments {
  /** a String can be added on a Form row as a TextCell */
  implicit def stringsAreTextCell(t: String) = new TextCell(t)  
  /** a Field can be added on a Form row as a FieldCell */
  implicit def fieldsAreTextCell(t: Field[_]) = new FieldCell(t)  
  /** a Prop can be added on a Form row as a PropCell */
  implicit def propsAreCell(t: Prop[_, _]) = new PropCell(t)
  /** a Form can be added on a Form row as a FormCell */
  implicit def formsAreCell(t: Form) = new FormCell(t)

  /** @return a new Form with the given title */
  def form(title: String) = Form(title)
  def form(title: String, lines: List[Form]) = {
    if (lines.isEmpty) Form(title)
    else {
      val header = lines(0).header match {
        case Nil => Form(title)
        case h :: rest => Form(title).tr(h, rest:_*) 
      }
      lines.foldLeft(header) { (res, cur) => res.tr(cur) }
    }
  }

  /** @return a new Field with no label and a value */
  def field[T](value: =>T): Field[T] = Field(value)
  /** @return a new Field with a label and a value */
  def field[T](label: String, value: =>T): Field[T] = Field(label, value)
  /** @return a new Field with a label and several values */
  def field(label: String, value1: Field[_], values: Field[_]*): Field[String] = Field(label, value1, values:_*)
  
  /** @return a new Prop with an actual value only */
  def prop[T](value: =>T) = new Prop[T, T](actual = Property(value))
  /** @return a new Prop with a label and an actual value only */
  def prop[T](label: String, actual: =>T) = Prop[T](label, actual)
  /** @return a new Prop with a label, an actual value and expected value */
  def prop[T, S](label: String, actual: =>Option[T], exp: =>Option[S]) = 
    new Prop[T, S](label, new Property(() => actual), new Property(() => exp))
  /** @return a new Prop with a label, an actual value and a constraint to apply to values */
  def prop[T, S](label: String, act: =>T, c: (T, S) => Result) = Prop(label, act, c)
  /** @return a new Prop with a label, an actual value and a matcher to apply to values */
  def prop[T, S](label: String, act: =>T, c: (S) => Matcher[T]) = Prop(label, act, c)
  
  def subset[T <: Any { def form: Form }](f1: List[T], f2: List[T]): List[Form] = {
    executeSubset(f1.map(_.form), f2.map(_.form))
  }
  def subsequence[T <: Any { def form: Form }](f1: List[T], f2: List[T]): List[Form] = {
    executeSubsequence(f1.map(_.form), f2.map(_.form))
  }
  def set[T <: Any { def form: Form }](f1: List[T], f2: List[T]): List[Form] = {
    executeSet(f1.map(_.form), f2.map(_.form))
  }
  def sequence[T <: Any { def form: Form }](f1: List[T], f2: List[T]): List[Form] = {
    executeSequence(f1.map(_.form), f2.map(_.form))
  }
  
  def executeSubset(form1: List[Form], form2: List[Form]): List[Form] = {
    val intersection = form1 intersect form2 
    intersection.map(_.setSuccess) ++
    (form1 diff intersection).map(_.setFailure)
  }

  def executeSubsequence(form1: List[Form], form2: List[Form]): List[Form] = {
    (form1 zip form2).foldLeft(Nil:List[Form]) { (res, cur) => cur match {
        case (f1, f2) if (f1 == f2) => res :+ f1.setSuccess
        case (f1, f2) => (res :+ f1.setFailure)
      }
    } ++
    form1.drop(form2.size).map(_.setFailure)
  }

  def executeSet(form1: List[Form], form2: List[Form]): List[Form] = {
    val intersection = form1 intersect form2 
    intersection.map(_.setSuccess) ++
    (form1 diff intersection).map(_.setFailure) ++
    (form2 diff intersection).map(_.setFailure)
  }

  def executeSequence(form1: List[Form], form2: List[Form]): List[Form] = {
    executeSubsequence(form1, form2) ++
    form2.drop(form1.size).map(_.setFailure)
  }
}
object Forms extends Forms
/**
 * Allow a Form to be inserted among Fragments as a Text Fragment
 * Allow a Form to be used as an example body and return a Result automatically
 */
trait FormFragments {
  class FormFragment(form: Form) extends specification.Text(FormCell(form).text)
  implicit def formsAreFragments(f: Form): Fragment = new FormFragment(f)
  implicit def formsAreExecutable(f: Form): Result = f.execute
}
object FormFragments extends FormFragments