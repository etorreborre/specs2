package org.specs2
package text

import control.Exceptions._
import text.Trim._
import util.parsing.combinator._

/**
 * this class extracts interpolated expressions from an interpolated string, given the string content and the text
 * pieces in between the interpolated expressions
 */
class Interpolated(stringContent: String, texts: Seq[String]) extends InterpolatedParsers {

  def expressions = {
   val parsed = parse(interpolatedString, stringContent).get
   if (parsed.size <= texts.size) parsed
   else                           parsed.take(texts.size - 1)
  }
}

trait InterpolatedParsers extends JavaTokenParsers {
  override def skipWhitespace = false
  lazy val interpolatedString: Parser[Seq[String]] = ((interpolatedVariable | noVariable)+) ^^ { _.filter(_.nonEmpty) }
  lazy val noVariable: Parser[String]              = "[^$]+".r ^^ { s => "" }
  lazy val interpolatedVariable: Parser[String]    = "$" ~> (ident | "{" ~> accoladeInsideExpression <~ "}")
  lazy val noAccolade: Parser[String]              = "[^\\{\\}]*".r
  lazy val accoladeInsideExpression: Parser[String]= { noAccolade ~ (accoladeExpression?) ~ noAccolade } ^^ {
    case n1 ~ accExp ~ n2 => n1+accExp.getOrElse("")+n2
  }
  lazy val accoladeExpression: Parser[String]      = { "{" ~ accoladeInsideExpression ~ "}" } ^^ {
    case start ~ accExp ~ end => start+accExp+end
  }
}
object InterpolatedParsers extends InterpolatedParsers
