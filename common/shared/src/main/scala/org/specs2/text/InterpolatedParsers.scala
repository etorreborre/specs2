package org.specs2.text

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

trait InterpolatedParsers extends JavaTokenParsers {
  override def skipWhitespace = false

  def interpolate(text: String): String =
    interpolatedString(new CharSequenceReader(text)) match {
      case Success(e, _) => e
      case Failure(m, _) => m
      case Error(m, _)   => m
    }

  def empty(p: Parser[String]) = p ^^ (_ => "")

  lazy val noVariable: Parser[String] = s"[^$${}]+".r.named("no variable")

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

