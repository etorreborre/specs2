package org.specs2
package form

import scala.xml._

private[specs2]
trait DecoratedProperties {
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
object DecoratedProperties extends DecoratedProperties