package org.specs2
package html

import io.FilePath

import scala.util.parsing.combinator._
import control.{Actions, Action}
import scalaz.std.anyVal._

/**
 * String template for HTML files using the Pandoc templating approach where variables to replace are enclosed with $$
 */
object HtmlTemplate {
  def runTemplate(template: String, variables: Map[String, String]): Action[String] = {
    val parser = pandocParser(variables)

    parser.parse(template) match {
      case parser.Success(s, _) => Actions.ok(s)
      case parser.Failure(e, _) => Actions.fail(e)
      case parser.Error(e, _)   => Actions.fail(e)
    }

  }

  /**
   * Variables replacement parser for Pandoc-like templates
   */
  def pandocParser(variables: Map[String, String]) = new RegexParsers {
    override def skipWhitespace = false

    lazy val template: Parser[String] =
      rep(conditional | block) ^^ (_.mkString)

    lazy val block: Parser[String] = rep1(variable | text) ^^ (_.mkString)

    lazy val variable: Parser[String] =
      ("$" ~> "[^\\$]+".r <~ "$").filter(v => !Seq("if(", "endif", "else").exists(v.startsWith)) ^^ { (v: String) => variables.getOrElse(v, "") }

    lazy val text: Parser[String] = regex("[^$]+".r)

    lazy val if1: Parser[String]   = "$if(" ~> "[^\\$\\)]+".r <~ ")$"
    lazy val else1: Parser[String] = "$else$"
    lazy val endif: Parser[String] = "$endif$"
    
    lazy val conditional = if1.flatMap { variable =>
      if (variables.contains(variable)) block <~ (else1 ~> block <~ endif)
      else                              (block ~ else1) ~> block <~ endif
    }
    
    def parse(string: String) = parseAll(template, string)
  }

}
