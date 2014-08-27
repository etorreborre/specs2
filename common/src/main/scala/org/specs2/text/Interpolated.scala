package org.specs2
package text

import util.parsing.combinator._
import Trim._
import util.parsing.input.{CharSequenceReader, Reader}

/**
 * This class extracts interpolated expressions from an interpolated string, given the string content and the text
 * pieces in between the interpolated expressions
 */
class Interpolated(stringContent: String, texts: Seq[String]) extends InterpolatedParsers {

  def expressions = {
    texts.zip(texts.drop(1)).foldLeft((stringContent, Seq[String]())) { case ((content, exps), (text, next)) =>
      val minusText = new String(content.drop(text.size).mkString).
                        replace("$$", "$") // in next, this replacement has already been done
      val textToParse = new String(if (minusText.indexOf(next) > 0) minusText.substring(0, minusText.indexOf(next)) else minusText)
      val fromDollar = textToParse.startFrom("$")
      val expression = interpolatedString(new CharSequenceReader(textToParse)) match {
        case Success(e, _) => e
        case Failure(m, _) => m
        case Error(m, _)   => m
      }
      (new String(minusText.drop(expression.size)), exps :+ trimVariableDeclaration(expression))
    }._2
  }

  private def trimVariableDeclaration = (_:String).removeStart("$").removeStart("{").removeEnd("}").removeStart("`").removeEnd("`")
}

trait InterpolatedParsers extends JavaTokenParsers {
  override def skipWhitespace = false

  def empty(p: Parser[String]) = p ^^ (_ => "")

  lazy val noVariable: Parser[String] = ("[^${}]+".r).named("no variable")

  lazy val interpolatedString: Parser[String] =
    interpolatedVariable |
    empty(noVariable)

  lazy val interpolatedVariable: Parser[String] =
    (("${" ~ interpolatedVariable ~ rep(interpolatedVariable) ~ "}") ^^ { case a~vv~b => a+vv.mkString+b }) |
    (("${" ~ noVariable ~ rep(interpolatedVariable) ~ "}") ^^ { case a~vv~b => a+vv.mkString+b }) |
    (("${" ~ quotedExpression ~ "}") ^^ { case a~vv~b => a+vv.toString+b }) |
      "$$" |
    (("$" ~ ident) ^^ { case a~b => a+b }) |
    multiline

  lazy val multiline: Parser[String] =
    (".+".r ~ "\\s*".r ~ rep(multiline)) ^^ { case l~s~rest => l+s+rest.mkString }

  lazy val quotedExpression: Parser[String] = (("`" ~ "[^`]+".r ~ "`") ^^ { case a~b~c => a+b+c }).named("quotedExpression")
}
object InterpolatedParsers extends InterpolatedParsers
