package org.specs2
package matcher

import java.util.regex._
import control.Exceptions._
import text.Quote._

/**
 * The `StringMatchers` trait provides matchers which are applicable to String objects
 */
trait StringMatchers extends StringBaseMatchers with StringBeHaveMatchers
object StringMatchers extends StringMatchers

/**
 * This trait provides base matchers for strings.
 *
 * IgnoreCase and ignoreSpace matchers are created by adapting the BeEqualTo matcher.
 */
private[specs2]
trait StringBaseMatchers { outer =>
  /** adapt the BeEqualTo matcher to provide ignoreCase and ignoreSpace matcher */
  implicit def stringMatcher(m: AdaptableMatcher[Any]): StringMatcher = new StringMatcher(m)
  class StringMatcher(m: AdaptableMatcher[Any]) {
    private val ignoringCase = (_:Any) + ", ignoring case"
    private val ignoringSpace = (_:Any) + ", ignoring space"
    def ignoreCase: AdaptableMatcher[Any] = m.^^^((s: Any) => s.toString.toLowerCase, ignoringCase, ignoringCase)
    def ignoreSpace: AdaptableMatcher[Any] = m.^^^((s: Any) => s.toString.trim, ignoringSpace, ignoringSpace)
  }

  /** matches if a.toLowerCase.trim = b.toLowerCase.trim */
  def ==/(s: String) = be_==/(s)
  /** matches if a.toLowerCase.trim = b.toLowerCase.trim */
  def be_==/(a: String) = new BeEqualTo(a).ignoreCase.ignoreSpace
  /** matches if a.toLowerCase.trim != b.toLowerCase.trim */
  def be_!=/(a: String) = new BeEqualTo(a).ignoreCase.ignoreSpace
  /** matches if a.toLowerCase.trim != b.toLowerCase.trim */
  def !=/(s: String) = be_!=/(s)
  /** matches if (b contains a) */
  def contain(t: String) = new Matcher[String] {
    def apply[S <: String](b: Expectable[S]) = {
      val a = t
      result(a != null && b.value != null && b.value.contains(a),
             b.description + " contains " + q(a),
             b.description + " doesn't contain " + q(a), b)
    }
  }
  /** matches if (b contains a) */
  def contain(t: Char) = new Matcher[String] {
    def apply[S <: String](b: Expectable[S]) = {
      val a = t
      result(b.value != null && b.value.contains(a),
             b.description + " contains " + q(a),
             b.description + " doesn't contain " + q(a), b)
    }
  }
  /** matches if b matches the regular expression a */
  def beMatching(t: =>String) = new BeMatching(t)
  /** alias for beMatching but matching just a fragment of the string*/
  def =~(t: =>String) = new BeMatching(".*"+t+".*")
  /** matches if b.startsWith(a) */
  def startWith(t: =>String) = new Matcher[String] {
    def apply[S <: String](b: Expectable[S]) = {
      val a = t
      result(b.value!= null && a!= null && b.value.startsWith(a),
             b.description + " starts with " + q(a),
             b.description + " doesn't start with " + q(a), b)
    }
  }
  /** matches if b.endsWith(a) */
  def endWith(t: =>String) = new Matcher[String] {
    def apply[S <: String](b: Expectable[S]) = {
      val a = t
      result(b.value!= null && a!= null && b.value.endsWith(a),
             b.description  + " ends with " + q(a),
             b.description  + " doesn't end with " + q(a), b)
    }
  }
  /** matches if the regexp a is found inside b */
  def find(a: String) = new FindMatcher(a)

  /**
   * Matcher to find if the regexp a is found inside b.
   * This matcher can be specialized to a FindMatcherWithGroups which will also check the found groups
   */
  class FindMatcher(t: =>String) extends Matcher[String] {
    def found(a: String, b: String) = {
      val matcher = Pattern.compile(a).matcher(b)
      matcher.find
    }
    def withGroup(group: String) = new FindMatcherWithGroups(t, group)
    def withGroups(groups: String*) = new FindMatcherWithGroups(t, groups:_*)
    def apply[S <: String](b: Expectable[S]) = {
      val a = t
      result(a != null && b.value != null && found(a, b.value),
             q(a) + " is found in " + b.description,
             q(a) + " isn't found in " + b.description, b)
      }
  }

  /**
   * Matcher to find if the regexp a is found inside b.
   * This matcher checks if the found groups are really the ones expected
   */
  class FindMatcherWithGroups(t: =>String, groups: String*) extends Matcher[String] {
    def found(a: String, b: String) = {
      val matcher = Pattern.compile(a).matcher(b)
      val groupsFound = new scala.collection.mutable.ListBuffer[String]()
      (1 to matcher.groupCount).foreach { i =>
        matcher.reset()
        while (matcher.find) { groupsFound += matcher.group(i) }
      }
      groupsFound.toList
    }
    def apply[S <: String](b: Expectable[S]) = {
      val a = t
      val groupsFound = found(a, b.value)
      val withGroups = if (groups.size > 1) " with groups " else " with group "
      def foundText = {
        if (groupsFound.isEmpty)
          ". Found nothing"
        else
           ". Found: " + q(groupsFound.mkString(", "))
      }
      val groupsToFind = if (groups == null) Nil else groups.toList
      result(a != null && b.value != null && groupsFound == groupsToFind,
             q(a) + " is found in " + b.description  + withGroups + q(groupsToFind.mkString(", ")),
             q(a) + " isn't found in " + b.description  + withGroups + q(groupsToFind.mkString(", ")) + foundText, b)
    }
  }

}

private[specs2]
trait StringBeHaveMatchers { outer: StringBaseMatchers =>
  implicit def toStringResultMatcher(result: MatchResult[String]) = new StringResultMatcher(result)
  class StringResultMatcher(result: MatchResult[String]) {
    def matching(s: String) = result(beMatching(s))
    def contain(s: String) = result(outer.contain(s))
    def containing(s: String) = result(outer.contain(s))
    def startWith(s: String) = result(outer.startWith(s))
    def endWith(s: String) = result(outer.endWith(s))
    def startingWith(s: String) = result(outer.startWith(s))
    def endingWith(s: String) = result(outer.endWith(s))
    def ==/(s: String) = result(outer.be_==/(s))
  }
  implicit def toNeutralStringMatcher(result: NeutralMatcher[Any]) : NeutralStringMatcher = new NeutralStringMatcher(result)
  class NeutralStringMatcher(result: NeutralMatcher[Any]) {
    def ==/(s: String) = outer.be_==/(s)
    def =~(s: String) = outer.=~(s)
  }
  implicit def toNotStringMatcher(result: NotMatcher[Any]) : NotStringMatcher = new NotStringMatcher(result)
  class NotStringMatcher(result: NotMatcher[Any]) {
    def ==/(s: String) = outer.be_==/(s).not
    def =~(s: String) = outer.=~(s).not
  }
  def matching(t: =>String) = beMatching(t)
  def containing(s: String) = outer.contain(s)
  def startingWith(s: String) = outer.startWith(s)
  def endingWith(s: String) = outer.endWith(s)
}
protected[specs2]
class BeMatching(t: =>String) extends Matcher[String] {
  def apply[S <: String](b: Expectable[S]) = {
    val a = t
    result(tryOr(b.value matches a){ (e: Exception) => false },
           b.description + " matches " + q(a),
           b.description + " doesn't match " + q(a), b)
  }
}

