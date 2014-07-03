package org.specs2
package text

import AnsiColors._
import ColorsMap._

/**
* This class defines the colors to use to print out text on the Console
* with defaults as AnsiColors for a dark background console
*/
class ConsoleColors extends AnsiColors with Colors {

  def textColor    = white
  def successColor = green
  def failureColor = yellow
  def errorColor   = red
  def pendingColor = cyan
  def skippedColor = magenta
  def statsColor   = cyan

  def text   (s: String, doIt: Boolean = true) = color(s, textColor,    doIt)
  def success(s: String, doIt: Boolean = true) = color(s, successColor, doIt)
  def failure(s: String, doIt: Boolean = true) = color(s, failureColor, doIt)
  def error  (s: String, doIt: Boolean = true) = color(s, errorColor,   doIt)
  def pending(s: String, doIt: Boolean = true) = color(s, pendingColor, doIt)
  def skipped(s: String, doIt: Boolean = true) = color(s, skippedColor, doIt)
  def stats  (s: String, doIt: Boolean = true) = color(s, statsColor,   doIt)

  def removeColors(s: String): String = AnsiColors.removeColors(s)

  override def toString = Seq(("text",   textColor),
                              ("success",successColor),
                              ("failure",failureColor),
                              ("error",  errorColor),
                              ("pending",pendingColor),
                              ("skipped",skippedColor),
                              ("stats",  statsColor)
                              ).map(p => p._1+": "+p._2)mkString("Colors(", ",", "}")
}

/**
 * This color scheme can be used with a white background
 */
class InvertedColors extends ConsoleColors {

  override def textColor    = black
  override def successColor = green
  override def failureColor = magenta
  override def errorColor   = red
  override def pendingColor = blue
  override def skippedColor = cyan
  override def statsColor   = blue

}

/**
 * This class takes colors from a map, using default colors if some values are missing
 */
case class MappedColors(colors: Map[String, String] = Map()) extends ConsoleColors { outer =>
  lazy val defaultColors = new ConsoleColors

  override lazy val textColor    = colors.getOrElse("text"   , defaultColors.textColor)
  override lazy val successColor = colors.getOrElse("success", defaultColors.successColor)
  override lazy val failureColor = colors.getOrElse("failure", defaultColors.failureColor)
  override lazy val errorColor   = colors.getOrElse("error"  , defaultColors.errorColor)
  override lazy val pendingColor = colors.getOrElse("pending", defaultColors.pendingColor)
  override lazy val skippedColor = colors.getOrElse("skipped", defaultColors.skippedColor)
  override lazy val statsColor   = colors.getOrElse("stats"  , defaultColors.statsColor)
}

/**
 * Factory method to create MappedColors 'colors' attribute
 */
object MappedColors {
  def fromArgs(args: String) = {
    val map = args.split(",").flatMap { s =>
      val keyValue = s.trim.split(":")
      if (keyValue.size == 2) getColor(keyValue(1).trim).map(c => keyValue(0) -> c)
      else                    None
    }

    if (args.contains("whitebg")) new MappedColors(Map(map:_*)) { override lazy val defaultColors = new InvertedColors }
    else                          new MappedColors(Map(map:_*))
  }
}

/**
 * Definition of abbreviated color names
 */
trait ColorsMap {
  val abbreviatedColors = Map (
    "w" -> white,
    "g" -> green,
    "y" -> yellow,
    "r" -> red,
    "be"-> blue,
    "c" -> cyan,
    "bk"-> black,
    "m" -> magenta
  )
  val colors = Map (
    "white"   -> white,
    "green"   -> green,
    "yellow"  -> yellow,
    "red"     -> red,
    "blue"    -> blue,
    "cyan"    -> cyan,
    "black"   -> black,
    "magenta" -> magenta
  )

  def getColor(s: String) = colors.get(s).orElse(abbreviatedColors.get(s))
}

object ColorsMap extends ColorsMap