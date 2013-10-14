package org.specs2
package form

/**
 * This trait defines functions that are applicable to any DecoratedProperty, i.e. something having a Decorator
 */
private[specs2]
trait DecoratedProperties {
  implicit def toDecorated[T <: DecoratedProperty[T]](d: T) = new Decorated(d)
  class Decorated[T <: DecoratedProperty[T]](d: T) {

    def code      = d.decoratorIs(d.decorator.code)
    def codeLabel = d.decoratorIs(d.decorator.codeLabel)
    def codeValue = d.decoratorIs(d.decorator.codeValue)

    def center      = d.decoratorIs(d.decorator.center)
    def centerLabel = d.decoratorIs(d.decorator.centerLabel)
    def centerValue = d.decoratorIs(d.decorator.centerValue)

    def right      = d.decoratorIs(d.decorator.right)
    def rightLabel = d.decoratorIs(d.decorator.rightLabel)
    def rightValue = d.decoratorIs(d.decorator.rightValue)

    def left      = d.decoratorIs(d.decorator.left)
    def leftLabel = d.decoratorIs(d.decorator.leftLabel)
    def leftValue = d.decoratorIs(d.decorator.leftValue)

    def bkColor(c: String)      = d.decoratorIs(d.decorator.bkColor(c))
    def bkColorLabel(c: String) = d.decoratorIs(d.decorator.bkColorLabel(c))
    def bkColorValue(c: String) = d.decoratorIs(d.decorator.bkColorValue(c))

    def color(c: String)      = d.decoratorIs(d.decorator.color(c))
    def colorLabel(c: String) = d.decoratorIs(d.decorator.colorLabel(c))
    def colorValue(c: String) = d.decoratorIs(d.decorator.colorValue(c))

    def white      = d.decoratorIs(d.decorator.white)
    def whiteLabel = d.decoratorIs(d.decorator.whiteLabel)
    def whiteValue = d.decoratorIs(d.decorator.whiteValue)

    def blue      = d.decoratorIs(d.decorator.blue)
    def blueLabel = d.decoratorIs(d.decorator.blueLabel)
    def blueValue = d.decoratorIs(d.decorator.blueValue)

    def red      = d.decoratorIs(d.decorator.red)
    def redLabel = d.decoratorIs(d.decorator.redLabel)
    def redValue = d.decoratorIs(d.decorator.redValue)

    def green = d.decoratorIs(d.decorator.green)
    def greenLabel = d.decoratorIs(d.decorator.greenLabel)
    def greenValue = d.decoratorIs(d.decorator.greenValue)

    def yellow      = d.decoratorIs(d.decorator.yellow)
    def yellowLabel = d.decoratorIs(d.decorator.yellowLabel)
    def yellowValue = d.decoratorIs(d.decorator.yellowValue)

    def bkWhite      = d.decoratorIs(d.decorator.bkWhite)
    def bkWhiteLabel = d.decoratorIs(d.decorator.bkWhiteLabel)
    def bkWhiteValue = d.decoratorIs(d.decorator.bkWhiteValue)

    def bkRed      = d.decoratorIs(d.decorator.bkRed)
    def bkRedLabel = d.decoratorIs(d.decorator.bkRedLabel)
    def bkRedValue = d.decoratorIs(d.decorator.bkRedValue)

    def bkBlue      = d.decoratorIs(d.decorator.bkBlue)
    def bkBlueLabel = d.decoratorIs(d.decorator.bkBlueLabel)
    def bkBlueValue = d.decoratorIs(d.decorator.bkBlueValue)

    def bkGreen      = d.decoratorIs(d.decorator.bkGreen)
    def bkGreenLabel = d.decoratorIs(d.decorator.bkGreenLabel)
    def bkGreenValue = d.decoratorIs(d.decorator.bkGreenValue)

    def bkYellow      = d.decoratorIs(d.decorator.bkYellow)
    def bkYellowLabel = d.decoratorIs(d.decorator.bkYellowLabel)
    def bkYellowValue = d.decoratorIs(d.decorator.bkYellowValue)

    def bkGrey      = d.decoratorIs(d.decorator.bkGrey)
    def bkGreyLabel = d.decoratorIs(d.decorator.bkGreyLabel)
    def bkGreyValue = d.decoratorIs(d.decorator.bkGreyValue)

    def bold      = d.decoratorIs(d.decorator.bold)
    def boldLabel = d.decoratorIs(d.decorator.boldLabel)
    def boldValue = d.decoratorIs(d.decorator.boldValue)

    def italics      = d.decoratorIs(d.decorator.italics)
    def italicsLabel = d.decoratorIs(d.decorator.italicsLabel)
    def italicsValue = d.decoratorIs(d.decorator.italicsValue)
  }
}
private[specs2]
object DecoratedProperties extends DecoratedProperties