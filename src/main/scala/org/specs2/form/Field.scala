package org.specs2
package form

import scala.xml._
import control.Exceptions._
import control.Property
import execute._
import DecoratedProperties._
/**
 * A Field is a property which is used only to display input values or output values.
 * 
 * The apply method can be used to retrieve the Field value:
 *   `Field(label, 1).apply() must_== 1`
 * 
 * The value is stored in a Property object so it will not be evaluated until explicitly
 * queried.
 */
case class Field[T](label: String, value: Property[T], decorator: Decorator = Decorator()) extends Executable with StandardResults
  with DecoratedProperty[Field[T]] {
  /** executing a field execute the value and returns success unless there is an Error */
  override def execute = {
    valueOrResult match {
      case Left(e)  => e
      case Right(v) => success
    }
  }
  def valueOrResult: Either[Result, T] = {
    trye(value.get)(Error(_))
  }
  /**
   * set a new value on the field. 
   */
  def apply(v: =>T) = new Field(label, value(v), decorator)
  /** @return the field value */
  def apply(): T = value.get
  /** alias for apply() */
  def get: T = apply()
  /** @return "label: value" */
  override def toString = if (label.nonEmpty) label + ": " + this.get else this.get.toString
  /** transforms this typed Field as a Field containing the toString value of the Fields value*/
  def toStringField = new Field(label, Property(value.get.toString), decorator)
  /** set a new Decorator */
  def decoratorIs(d: Decorator) = copy(decorator = d)

  /** use this Field as a header in a table */
  def header = this.center.bold.bkGrey

  override def equals(a: Any) = a match {
    case Field(l, v, _) => label == l && value == v
    case other          => false
  }
  override def hashCode = label.hashCode + value.hashCode
}
/**
 * Factory methods for creating Fields. Fields values can also be concatenated to produce
 * "summary" fields.
 * 
 * val f1 = Field(label, "hello")
 * val f2 = Field(label, "world")
 * val concatenatedFields = Field(label, f1, f2)
 * concatenatedFields.toString == label: hello/world
 * 
 * val concatenatedFields2 = Field(label, ", ", f1, f2)
 * concatenatedFields2.toString == label: hello, world
 */
case object Field {
  def apply[T](value: =>T): Field[T] = new Field("", Property(value))
  def apply[T](label: String, value: =>T): Field[T] = new Field(label, Property(value))
  def apply(label: String, value1: Field[_], values: Field[_]*): Field[String] = Field(label, "/", value1, values:_*)
  def apply(label: String, separator: String, value1: Field[_], values: Field[_]*): Field[String] = {
    val f: Field[String] = if (values.isEmpty)
      Field(label, value1.get.toString)
    else
      Field(label, (value1 :: values.toList).map(_.get).mkString(separator))
    Field(label, f())
  }
}

trait DecoratedProperty[T] extends DecoratedLabel[T] {
  /** set a new Decorator */
  def decorateWith(f: Any => Any) = decoratorIs(decorator.decorateWith(f))
  /** set a new Decorator for the value */
  def decorateValueWith(f: Any => Any) = decoratorIs(decorator.decorateValueWith(f))
  /** set a new style */
  def styleWith(s: (String, String)) = decoratorIs(decorator.styleWith(s))
  /** set a new style for the value */
  def styleValueWith(s: (String, String)) = decoratorIs(decorator.styleValueWith(s))
  /** do the decoration */
  def decorateValue(ns: Any) = decorator.value(ns)

  def valueStyles = decorator.valueStyles.mkString("; ")
}
trait DecoratedLabel[T] {
  val decorator: Decorator
  /** set a new Decorator */
  def decoratorIs(d: Decorator): T
  /** set a new Decorator for the label */
  def decorateLabelWith(f: Any => Any) = decoratorIs(decorator.decorateLabelWith(f))
  /** set a new style for the label */
  def styleLabelWith(s: (String, String)) = decoratorIs(decorator.styleLabelWith(s))
  /** do the decoration */
  def decorateLabel(ns: Any) = decorator.label(ns)
  /** return the label styles */
  def labelStyles = decorator.labelStyles.mkString("; ")
}
case class Decorator(label: Any => Any = identity, value: Any => Any = identity,
                     labelStyles: List[String] = Nil, valueStyles: List[String] = Nil) {
  def decorateWith(f: Any => Any) = copy(label = f compose label, value = f compose value)
  def decorateLabelWith(f: Any => Any) = copy(label = f compose label)
  def decorateValueWith(f: Any => Any) = copy(value = f compose value)

  def styleWith     (s: (String, String)) = copy(labelStyles = labelStyles :+ (s._1+":"+s._2), valueStyles = valueStyles :+ (s._1+":"+s._2))
  def styleLabelWith(s: (String, String)) = copy(labelStyles = labelStyles :+ (s._1+":"+s._2))
  def styleValueWith(s: (String, String)) = copy(valueStyles = valueStyles :+ (s._1+":"+s._2))

  def code = decorateWith((ns: Any) => <code class="prettyprint">{ns}</code>)
  def codeLabel = decorateLabelWith((ns: Any) => <code class="prettyprint">{ns}</code>)
  def codeValue = decorateValueWith((ns: Any) => <code class="prettyprint">{ns}</code>)

  def center = styleWith("text-align"->"center")
  def centerLabel = styleLabelWith("text-align"->"center")
  def centerValue = styleValueWith("text-align"->"center")

  def right = styleWith("text-align"->          "right")
  def rightLabel = styleLabelWith("text-align"->"right")
  def rightValue = styleValueWith("text-align"->"right")

  def left = styleWith("text-align"->          "left")
  def leftLabel = styleLabelWith("text-align"->"left")
  def leftValue = styleValueWith("text-align"->"left")

  def bkColor(c: String) = styleWith("background-color"->c)
  def bkColorLabel(c: String) = styleLabelWith("background-color"->c)
  def bkColorValue(c: String) = styleValueWith("background-color"->c)

  def color(c: String) = styleWith("color"->c)
  def colorLabel(c: String) = styleLabelWith("color"->c)
  def colorValue(c: String) = styleValueWith("color"->c)

  def white      = color     ("#FFFFFF")
  def whiteLabel = colorLabel("#FFFFFF")
  def whiteValue = colorValue("#FFFFFF")

  def blue      = color("#1E90FF")
  def blueLabel = colorLabel("#1E90FF")
  def blueValue = colorValue("#1E90FF")

  def red = color("#FF9999")
  def redLabel = colorLabel("#FF9999")
  def redValue = colorValue("#FF9999")

  def green = color("#CCFFCC")
  def greenLabel = colorLabel("#CCFFCC")
  def greenValue = colorValue("#CCFFCC")

  def yellow      = color("#FFFF99")
  def yellowLabel = colorLabel("#FFFF99")
  def yellowValue = colorValue("#FFFF99")

  def bkWhite      = bkColor     ("#FFFFFF")
  def bkWhiteLabel = bkColorLabel("#FFFFFF")
  def bkWhiteValue = bkColorValue("#FFFFFF")

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

  def bold = decorateWith((ns: Any) => <b>{ns}</b>)
  def boldLabel = decorateLabelWith((ns: Any) => <b>{ns}</b>)
  def boldValue = decorateValueWith((ns: Any) => <b>{ns}</b>)

  def italics = decorateWith((ns: Any) => <i>{ns}</i>)
  def italicsLabel = decorateLabelWith((ns: Any) => <i>{ns}</i>)
  def italicsValue = decorateValueWith((ns: Any) => <i>{ns}</i>)

}