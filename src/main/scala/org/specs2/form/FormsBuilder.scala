package org.specs2
package form

import control.Property
import execute._
import matcher._
import scala.xml.NodeSeq

/**
 * Utility methods to build Fields, Props and Forms and insert them in other Forms or 
 * Fragments.
 */
private[specs2]
trait FormsBuilder {

  /** anything can be added on a Form row as a TextCell */
  implicit def anyAreTextCell(t: Any) = new TextCell(t.toString)
  /** a Field can be added on a Form row as a FieldCell */
  implicit def fieldsAreTextCell(t: Field[_]) = new FieldCell(t)  
  /** a Prop can be added on a Form row as a PropCell */
  implicit def propsAreCell(t: Prop[_, _]) = new PropCell(t)
  /** a Form can be added on a Form row as a FormCell */
  implicit def formsAreCell(t: Form) = new FormCell(t)
  implicit def formsAreExecutable(f: Form): Result = f.execute

  /** @return a new Form with the given title */
  def form(title: String) = Form(title)

  /** @return a new Form with the given title and rows */
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
  def prop[T, S](label: String, actual: =>T, exp: =>S) =
    new Prop[T, S](label, new Property(() => Some(actual)), new Property(() => Some(exp)))
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

  implicit def toDecorated[T <: DecoratedProperty[T]](d: T) = new Decorated(d)
  class Decorated[T <: DecoratedProperty[T]](d: T) {
    def code = d.decorateWith((ns: Any) => <code class="prettyprint">{ns}</code>)
    def codeLabel = d.decorateLabelWith((ns: Any) => <code class="prettyprint">{ns}</code>)
    def codeValue = d.decorateValueWith((ns: Any) => <code class="prettyprint">{ns}</code>)

    def center = d.styleWith("text-align"->"center")
    def centerLabel = d.styleLabelWith("text-align"->"center")
    def centerValue = d.styleValueWith("text-align"->"center")

    def right = d.styleWith("text-align"->          "right")
    def rightLabel = d.styleLabelWith("text-align"->"right")
    def rightValue = d.styleValueWith("text-align"->"right")

    def left = d.styleWith("text-align"->          "left")
    def leftLabel = d.styleLabelWith("text-align"->"left")
    def leftValue = d.styleValueWith("text-align"->"left")

    def bkColor(c: String) = d.styleWith("background-color"->c)
    def bkColorLabel(c: String) = d.styleLabelWith("background-color"->c)
    def bkColorValue(c: String) = d.styleValueWith("background-color"->c)

    def color(c: String) = d.styleWith("color"->c)
    def colorLabel(c: String) = d.styleLabelWith("color"->c)
    def colorValue(c: String) = d.styleValueWith("color"->c)

    def blue = color("#1E90FF")
    def blueLabel = colorLabel("#1E90FF")
    def blueValue = colorValue("#1E90FF")

    def red = color("#FF9999")
    def redLabel = colorLabel("#FF9999")
    def redValue = colorValue("#FF9999")

    def green = color("#CCFFCC")
    def greenLabel = colorLabel("#CCFFCC")
    def greenValue = colorValue("#CCFFCC")

    def bkBlue = bkColor("#1E90FF")
    def bkBlueLabel = bkColorLabel("#1E90FF")
    def bkBlueValue = bkColorValue("#1E90FF")

    def bkRed = bkColor("#FF9999")
    def bkRedLabel = bkColorLabel("#FF9999")
    def bkRedValue = bkColorValue("#FF9999")

    def bkGreen = bkColor("#CCFFCC")
    def bkGreenLabel = bkColorLabel("#CCFFCC")
    def bkGreenValue = bkColorValue("#CCFFCC")

    def bkYellow = bkColor("#FFFF99")
    def bkYellowLabel = bkColorLabel("#FFFF99")
    def bkYellowValue = bkColorValue("#FF9999")

    def bkGrey = bkColor("#EEEEEE")
    def bkGreyLabel = bkColorLabel("#EEEEEE")
    def bkGreyValue = bkColorValue("#EEEEEE")

    def bold = d.decorateWith((ns: Any) => <b>{ns}</b>)
    def boldLabel = d.decorateLabelWith((ns: Any) => <b>{ns}</b>)
    def boldValue = d.decorateValueWith((ns: Any) => <b>{ns}</b>)

    def italics = d.decorateWith((ns: Any) => <i>{ns}</i>)
    def italicsLabel = d.decorateLabelWith((ns: Any) => <i>{ns}</i>)
    def italicsValue = d.decorateValueWith((ns: Any) => <i>{ns}</i>)
  }
}
private[specs2]
object FormsBuilder extends FormsBuilder
