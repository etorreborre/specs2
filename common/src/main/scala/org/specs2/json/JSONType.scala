package org.specs2
package json

/**
 * COPIED from the standard lib for convenience. Internal use only
 */

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.input.CharArrayReader._

/**
 *  A marker class for the JSON result types.
 *
 *  @author Derek Chen-Becker <"java"+@+"chen-becker"+"."+"org">
 */
private[specs2]
sealed abstract class JSONType {
  /**
   * This version of toString allows you to provide your own value
   * formatter.
   */
  def toString (formatter : JSONFormat.ValueFormatter) : String

  /**
   * Returns a String representation of this JSON value
   * using the JSONFormat.defaultFormatter.
   */
  override def toString = toString(JSONFormat.defaultFormatter)
}

/**
 * This object defines functions that are used when converting JSONType
 * values into String representations. Mostly this is concerned with
 * proper quoting of strings.
 *
 * @author Derek Chen-Becker <"java"+@+"chen-becker"+"."+"org">
 */
private[specs2]
object JSONFormat {
  /**
   * This type defines a function that can be used to
   * format values into JSON format.
   */
  type ValueFormatter = Any => String

  /**
   * The default formatter used by the library. You can
   * provide your own with the toString calls on
   * JSONObject and JSONArray instances.
   */
  val defaultFormatter : ValueFormatter = {
    case s : String => "\"" + quoteString(s) + "\""
    case jo : JSONObject => jo.toString(defaultFormatter)
    case ja : JSONArray => ja.toString(defaultFormatter)
    case other => other.toString
  }

  /**
   * This function can be used to properly quote Strings
   * for JSON output.
   */
  def quoteString (s : String) : String =
  s.map {
    case '"'  => "\\\""
    case '\\' => "\\\\"
    case '/'  => "\\/"
    case '\b' => "\\b"
    case '\f' => "\\f"
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\t' => "\\t"
    /* We'll unicode escape any control characters. These include:
     * 0x0 -> 0x1f  : ASCII Control (C0 Control Codes)
     * 0x7f         : ASCII DELETE
     * 0x80 -> 0x9f : C1 Control Codes
     *
     * Per RFC4627, section 2.5, we're not technically required to
     * encode the C1 codes, but we do to be safe.
     */
    case c if ((c >= '\u0000' && c <= '\u001f') || (c >= '\u007f' && c <= '\u009f')) => "\\u%04x".format(c.toInt)
    case c => c
  }.mkString
}

/**
 *  Represents a JSON Object (map).
 *
 *  @author Derek Chen-Becker <"java"+@+"chen-becker"+"."+"org">
 */
private[specs2]
case class JSONObject (obj : Map[String,Any]) extends JSONType {
  def toString (formatter : JSONFormat.ValueFormatter) =
    "{" + obj.map({ case (k,v) => formatter(k.toString) + " : " + formatter(v) }).mkString(", ") + "}"
}

/**
 *  Represents a JSON Array (list).
 *  @author Derek Chen-Becker <"java"+@+"chen-becker"+"."+"org">
 */
private[specs2]
case class JSONArray (list : List[Any]) extends JSONType {
  def toString (formatter : JSONFormat.ValueFormatter) =
    "[" + list.map(formatter).mkString(", ") + "]"
}

/**
 *  The main JSON Parser.
 *
 *  @author Derek Chen-Becker <"java"+@+"chen-becker"+"."+"org">
 */
private[specs2]
class Parser extends StdTokenParsers with ImplicitConversions {
  // Fill in abstract defs
  type Tokens = Lexer
  val lexical = new Tokens

  // Configure lexical parsing
  lexical.reserved ++= List("true", "false", "null")
  lexical.delimiters ++= List("{", "}", "[", "]", ":", ",")

  /** Type signature for functions that can parse numeric literals */
  type NumericParser = String => Any

  // Global default number parsing function
  protected var defaultNumberParser : NumericParser = {_.toDouble}

  // Per-thread default number parsing function
  protected val numberParser = new ThreadLocal[NumericParser]() {
    override def initialValue() = defaultNumberParser
  }

  // Define the grammar
  def root       = jsonObj | jsonArray
  def jsonObj    = "{" ~> repsep(objEntry, ",") <~ "}" ^^ { case vals : List[_] => JSONObject(Map(vals : _*)) }
  def jsonArray  = "[" ~> repsep(value, ",") <~ "]" ^^ { case vals : List[_] => JSONArray(vals) }
  def objEntry   = stringVal ~ (":" ~> value) ^^ { case x ~ y => (x, y) }
  def value: Parser[Any] = (jsonObj | jsonArray | number | "true" ^^^ true | "false" ^^^ false | "null" ^^^ null | stringVal)
  def stringVal  = accept("string", { case lexical.StringLit(n) => n} )
  def number     = accept("number", { case lexical.NumericLit(n) => numberParser.get.apply(n)} )
}

private[specs2]
class Lexer extends StdLexical with ImplicitConversions {

  override def token: Parser[Token] =
  //( '\"' ~ rep(charSeq | letter) ~ '\"' ^^ lift(StringLit)
    ( string ^^ StringLit
      | number ~ letter ^^ { case n ~ l => ErrorToken("Invalid number format : " + n + l) }
      | '-' ~> whitespace ~ number ~ letter ^^ { case ws ~ num ~ l => ErrorToken("Invalid number format : -" + num + l) }
      | '-' ~> whitespace ~ number ^^ { case ws ~ num => NumericLit("-" + num) }
      | number ^^ NumericLit
      | EofCh ^^^ EOF
      | delim
      | '\"' ~> failure("Unterminated string")
      | rep(letter) ^^ checkKeyword
      | failure("Illegal character")
      )

  def checkKeyword(xs : List[Any]) = {
    val strRep = xs mkString ""
    if (reserved contains strRep) Keyword(strRep) else ErrorToken("Not a keyword: " + strRep)
  }

  /** A string is a collection of zero or more Unicode characters, wrapped in
   *  double quotes, using backslash escapes (cf. http://www.json.org/).
   */
  def string = '\"' ~> rep(charSeq | chrExcept('\"', '\n', EofCh)) <~ '\"' ^^ { _ mkString "" }

  override def whitespace = rep(whitespaceChar)

  def number = intPart ~ opt(fracPart) ~ opt(expPart) ^^ { case i ~ f ~ e =>
    i + optString(".", f) + optString("", e)
  }
  def intPart = zero | intList
  def intList = nonzero ~ rep(digit) ^^ {case x ~ y => (x :: y) mkString ""}
  def fracPart = '.' ~> rep(digit) ^^ { _ mkString "" }
  def expPart = exponent ~ opt(sign) ~ rep1(digit) ^^ { case e ~ s ~ d =>
    e + optString("", s) + d.mkString("")
  }

  private def optString[A](pre: String, a: Option[A]) = a match {
    case Some(x) => pre + x.toString
    case None => ""
  }

  def zero: Parser[String] = '0' ^^^ "0"
  def nonzero = elem("nonzero digit", d => d.isDigit && d != '0')
  def exponent = elem("exponent character", d => d == 'e' || d == 'E')
  def sign = elem("sign character", d => d == '-' || d == '+')

  def charSeq: Parser[String] =
    ('\\' ~ '\"' ^^^ "\""
      |'\\' ~ '\\' ^^^ "\\"
      |'\\' ~ '/'  ^^^ "/"
      |'\\' ~ 'b'  ^^^ "\b"
      |'\\' ~ 'f'  ^^^ "\f"
      |'\\' ~ 'n'  ^^^ "\n"
      |'\\' ~ 'r'  ^^^ "\r"
      |'\\' ~ 't'  ^^^ "\t"
      |'\\' ~> 'u' ~> unicodeBlock)

  val hexDigits = Set[Char]() ++ "0123456789abcdefABCDEF".toArray
  def hexDigit = elem("hex digit", hexDigits.contains(_))

  private def unicodeBlock = hexDigit ~ hexDigit ~ hexDigit ~ hexDigit ^^ {
    case a ~ b ~ c ~ d =>
      new String(Array(Integer.parseInt(List(a, b, c, d) mkString "", 16)), 0, 1)
  }
}
