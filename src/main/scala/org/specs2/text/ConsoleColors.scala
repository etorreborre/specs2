package org.specs2
package text

import AnsiColors._

/**
* This class defines the colors to use to print out text on the Console
* with defaults as AnsiColors for a dark background console
*/
class ConsoleColors extends AnsiColors with Colors {

  def textColor    = white
  def successColor = green
  def failureColor = yellow
  def errorColor   = red
  def pendingColor = blue
  def skippedColor = cyan
  def statsColor   = blue

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

import main.SystemProperties

class ColorsFromSystemProperties() extends ConsoleColors with ColorsMap {
  lazy val properties: SystemProperties = SystemProperties
  lazy val defaultColors = properties.getIfElse("color.whitebg", new InvertedColors: ConsoleColors)(new ConsoleColors)

  override lazy val textColor    = getColor(properties.getOrElse("color.text",    "notfound")).getOrElse(defaultColors.textColor   )
  override lazy val successColor = getColor(properties.getOrElse("color.success", "notfound")).getOrElse(defaultColors.successColor)
  override lazy val failureColor = getColor(properties.getOrElse("color.failure", "notfound")).getOrElse(defaultColors.failureColor)
  override lazy val errorColor   = getColor(properties.getOrElse("color.error",   "notfound")).getOrElse(defaultColors.errorColor  )
  override lazy val pendingColor = getColor(properties.getOrElse("color.pending", "notfound")).getOrElse(defaultColors.pendingColor)
  override lazy val skippedColor = getColor(properties.getOrElse("color.skipped", "notfound")).getOrElse(defaultColors.skippedColor)
  override lazy val statsColor   = getColor(properties.getOrElse("color.stats",   "notfound")).getOrElse(defaultColors.statsColor  )
}

/**
 * This class checks if colors must be taken from system properties
 */
class SmartColors(argsColors: Map[String, String] = Map()) extends ConsoleColors with ColorsMap with SystemProperties { outer =>
  lazy val defaultColors = new ConsoleColors
  lazy val systemColors = new ColorsFromSystemProperties {
    override lazy val properties = outer
  }

  lazy val fromSystemProperties = areDefined("color\\..*")

  override lazy val textColor    = if (argsColors.get("text"   ).isDefined) argsColors("text"   ) else if (fromSystemProperties) systemColors.textColor    else defaultColors.textColor
  override lazy val successColor = if (argsColors.get("success").isDefined) argsColors("success") else if (fromSystemProperties) systemColors.successColor else defaultColors.successColor
  override lazy val failureColor = if (argsColors.get("failure").isDefined) argsColors("failure") else if (fromSystemProperties) systemColors.failureColor else defaultColors.failureColor
  override lazy val errorColor   = if (argsColors.get("error"  ).isDefined) argsColors("error"  ) else if (fromSystemProperties) systemColors.errorColor   else defaultColors.errorColor
  override lazy val pendingColor = if (argsColors.get("pending").isDefined) argsColors("pending") else if (fromSystemProperties) systemColors.pendingColor else defaultColors.pendingColor
  override lazy val skippedColor = if (argsColors.get("skipped").isDefined) argsColors("skipped") else if (fromSystemProperties) systemColors.skippedColor else defaultColors.skippedColor
  override lazy val statsColor   = if (argsColors.get("stats"  ).isDefined) argsColors("stats"  ) else if (fromSystemProperties) systemColors.statsColor   else defaultColors.statsColor
}

/**
 * Factory method to create SmartColors 'argsColor' attribute
 */
private[specs2]
object SmartColors extends ColorsMap {
  def fromArgs(args: String) = {
    val map = args.split(",").flatMap { s =>
      val keyValue = s.trim.split(":")
      if (keyValue.size == 2)
         getColor(keyValue(1).trim).map(c => keyValue(0) -> c)
      else
        None
    }
    if (args.contains("whitebg"))
      new SmartColors(Map(map:_*)) { override lazy val defaultColors = new InvertedColors }
    else
      new SmartColors(Map(map:_*))
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