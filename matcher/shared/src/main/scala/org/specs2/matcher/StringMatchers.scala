package org.specs2
package matcher

import java.util.regex.{Matcher => _, Pattern, PatternSyntaxException}
import text.Quote._
import text.Regexes._
import control.Exceptions._
import execute.Result._
import util.matching.Regex
import StringMatchers._

/**
 * The `StringMatchers` trait provides matchers which are applicable to String objects
 */
trait StringMatchers:

  /** adapt the BeEqualTo matcher to provide ignoreCase and ignoreSpace matcher */
  extension (m: AdaptableMatcher[Any])

    def ignoreCase: AdaptableMatcher[Any] =
      m.^^^((s: Any) => s.toString.toLowerCase, ignoringCase, ignoringCase)

    def ignoreSpace: AdaptableMatcher[Any] =
      m.^^^((s: Any) => s.toString.replaceAll("\\s", ""), ignoringSpace, ignoringSpace)

    def trimmed: AdaptableMatcher[Any] =
      m.^^^((s: Any) => s.toString.trim, isTrimmed, isTrimmed)

  private[specs2] val ignoringCase = (_:Any).toString + ", ignoring case"
  private[specs2] val ignoringSpace = (_:Any).toString + ", ignoring space"
  private[specs2] val isTrimmed = (_:Any).toString + ", trimmed"

  /** matches if a.toLowerCase.trim = b.toLowerCase.trim */
  def ==/(s: String): Matcher[String] =
    be_==/(s)

  /** matches if a.toLowerCase.trim = b.toLowerCase.trim */
  def be_==/(a: String): Matcher[String] =
    new BeEqualTo(a).ignoreCase.ignoreSpace

  /** matches if a.toLowerCase.trim != b.toLowerCase.trim */
  def be_!=/(a: String): Matcher[String] =
    be_==/(a).not

  /** matches if a.toLowerCase.trim != b.toLowerCase.trim */
  def !=/(s: String): Matcher[String] =
    be_!=/(s)

  /** matches if (b contains a) */
  def contain(t: String): Matcher[String] =
    new Matcher[String]:
      def apply[S <: String](b: Expectable[S]) =
        val a = t
        result(a != null && b.value != null && b.value.contains(a),
               b.description + " doesn't contain " + q(a))

  /** matches if (b contains a) */
  def contain(t: Char): Matcher[String] =
    new Matcher[String]:
      def apply[S <: String](b: Expectable[S]) =
        val a = t
        result(b.value != null && b.value.contains(a),
               b.description + " doesn't contain " + q(a))

  /** matches if b matches the regular expression a */
  def beMatching[T : MatchingExpression](t: T): Matcher[String] =
    new BeMatching(summon[MatchingExpression[T]].toPattern(t))

  /** alias to use with contain */
  def matching[T : MatchingExpression](t: T): Matcher[String] =
    beMatching(t)

  /** alias for beMatching but matching just a fragment of the string */
  def =~[T : MatchingExpression](t: T): Matcher[String] =
    BeMatching.withPart[T](t)(using summon[MatchingExpression[T]])

  /** matches if b.startsWith(a) */
  def startWith(a: String): Matcher[String] =
    new Matcher[String]:
      def apply[S <: String](b: Expectable[S]) =
        result(b.value != null && a != null && b.value.startsWith(a),
               s"${b.description} doesn't start with ${q(a)}")

  /** matches if b.endsWith(a) */
  def endWith(t: =>String): Matcher[String] =
    new Matcher[String]:
      def apply[S <: String](b: Expectable[S]) =
        val a = t
        result(b.value!= null && a!= null && b.value.endsWith(a),
               b.description  + " doesn't end with " + q(a))
  /** matches if the regexp a is found inside b */
  def find(a: =>String): FindMatcher =
    new FindMatcher(a)

  /** matches if the pattern p is found inside b */
  def find(p: Pattern): FindMatcherPattern =
    new FindMatcherPattern(p)

  /** matches if the regexp r is found inside b */
  def find(r: Regex): FindMatcherRegex =
    new FindMatcherRegex(r)

  /**
   * Matcher to find if the regexp a is found inside b.
   * This matcher can be specialized to a FindMatcherWithGroups which will also check the found groups
   */
  class FindMatcher(t: =>String) extends Matcher[String]:
    lazy val pattern = Pattern.compile(t)

    def withGroup(group: String) = new FindMatcherWithGroups(t, group)
    def withGroups(groups: String*) = new FindMatcherWithGroups(t, groups:_*)
    def apply[S <: String](b: Expectable[S]) =
      val a = t
      result(a != null && b.value != null && pattern.matcher(b.value).find,
             q(a) + " isn't found in " + b.description)

  /**
   * Matcher to find if the pattern p is found inside b.
   */
  class FindMatcherPattern(p: Pattern) extends FindMatcher(p.toString):
    override lazy val pattern = p
    override def withGroup(group: String) = new FindMatcherPatternWithGroups(p, group)
    override def withGroups(groups: String*) = new FindMatcherPatternWithGroups(p, groups:_*)
  /**
   * Matcher to find if the Regex r is found inside b.
   */
  class FindMatcherRegex(r: Regex) extends FindMatcherPattern(r.pattern)

  /**
   * Matcher to find if the regexp a is found inside b.
   * This matcher checks if the found groups are really the ones expected
   */
  class FindMatcherWithGroups(t: =>String, groups: String*) extends Matcher[String]:
    lazy val pattern = Pattern.compile(t)

    def found(b: String) =
      val matcher = pattern.matcher(b)
      val groupsFound = new scala.collection.mutable.ListBuffer[String]()
      (1 to matcher.groupCount).foreach { i =>
        matcher.reset()
        while matcher.find do { groupsFound += matcher.group(i) }
      }
      groupsFound.toList
    def apply[S <: String](b: Expectable[S]) =
      val a = t
      val groupsFound = found(b.value)
      val withGroups = if groups.size > 1 then " with groups " else " with group "
      def foundText =
        if groupsFound.isEmpty then
          ". Found nothing"
        else
           ". Found: " + q(groupsFound.mkString(", "))
      val groupsToFind = if groups == null then Nil else groups.toList
      result(a != null && b.value != null && groupsFound == groupsToFind,
             q(a) + " isn't found in " + b.description  + withGroups + q(groupsToFind.mkString(", ")) + foundText)
  /**
   * Matcher to find if the pattern p is found inside b.
   */
  class FindMatcherPatternWithGroups(p: Pattern, groups: String*) extends FindMatcherWithGroups(p.toString, groups:_*):
    override lazy val pattern = p

  given MatchingExpression[String] with
    def toPattern(s: String): Pattern =
      try Pattern.compile(s)
      catch { case e: PatternSyntaxException => Pattern.compile(Pattern.quote(s)) }

  given MatchingExpression[Pattern] with
    def toPattern(p: Pattern): Pattern =
      p

  given MatchingExpression[Regex] with
    def toPattern(r: Regex): Pattern =
      r.pattern

object StringMatchers extends StringMatchers

trait MatchingExpression[T]:
  def toPattern(t: T): Pattern

protected[specs2]
class BeMatching(pattern: Pattern) extends Matcher[String]:

  def apply[S <: String](b: Expectable[S]) =
    result(tryOrElse(pattern.matcher(b.value).matches)(false),
           s"'${b.description}' doesn't match ${q(pattern)}")

object BeMatching extends StringMatchers:

  def withPart[T : MatchingExpression](t: T): BeMatching =
    val pattern = summon[MatchingExpression[T]].toPattern(t)
    new BeMatching(Pattern.compile(pattern.toString.regexPart, pattern.flags()))
