package org.specs2
package form

/**
 * This class contains functions to decorate and style a label and a value:
 *
 *  * with a function taking the xml for the label/value and returning some xml
 *  * with some xml attributes "name":"value" to style those labels/values
 *
 * The methods of that class allow to stack new decoration, new styling attributes but also define standard decoration
 * and styles for bold / italic / centered ... text.
 */
case class Decorator(label: Any => Any = identity,
                     value: Any => Any = identity,
                     labelStyles: List[String] = Nil,
                     valueStyles: List[String] = Nil) {

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