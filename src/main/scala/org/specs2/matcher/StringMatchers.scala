package org.specs2
package matcher
import AnyMatchers._
import java.util.regex._
/**
 * The <code>StringMatchers</code> trait provides matchers which are applicable to String objects
 */
trait StringMatchers { outer =>
  
  /**
   * Matches if (a.equalsIgnoreCase(b))
   */   
  def beEqualToIgnoringCase[T <: String](a: T) = new BeEqualToIgnoringCase(a)
  /**
   * Matches if (a.equalsIgnoreCase(b))
   */   
  def be_==/[T <: String](a: T) = beEqualToIgnoringCase(a)  
  /**
   * Matches if (a.equalsIgnoreCase(b))
   * @deprecated use equalToIgnoringCase instead
   */   
  def equalIgnoreCase[T <: String](a: T) = beEqualToIgnoringCase(a)

  /**
   * Matches if (a.trim == b.trim)
   */   
  def beEqualToIgnoringSpace[T <: String](t: =>T) = new Matcher[T] { 
    def apply(v: => T)(d: Any => String) = {
      val (a, b) = (t, v) 
      result(a != null && b != null && a.trim == b.trim, 
             d(b) + " is equal ignoring space to " + q(a), 
             d(b) + " is not equal ignoring space to " + q(a))
      }
  }

  /**
   * Matches if (a.trim == b.trim)
   * @deprecated use beEqualToIgnoringSpace instead
   */   
  def equalIgnoreSpace[T <: String](a: =>T) = beEqualToIgnoringSpace(a)
  /**
   * Matches if (b.indexOf(a) >= 0)
   */   
  def include[T <: String](t: =>String) = new Matcher[T] { 
    def apply(v: => T)(d: Any => String) = {
      val (a, b) = (t, v)
      result(a != null && b != null && b.indexOf(a) >= 0, 
             d(b) + " includes " + q(a), 
             d(b) + " doesn't include " + q(a))
    } 
  }

  /**
   * Matches if b matches the regular expression a
   */   
  def beMatching[T <: String](t: =>T) = new Matcher[T] {
    def apply(v: => T)(d: Any => String) = {
      val (a, b) = (t, v)
      result(a matches b, 
             d(b) + " matches " + q(a), 
             d(b) + " doesn't match " + q(a))
    }
  }

  /**
   * Matches if b.startsWith(a)
   */   
  def startWith[T <: String](t: =>T) = new Matcher[T] { 
    def apply(v: =>T)(d: Any => String) = {
      val (a, b) = (t, v)
      result(b!= null && a!= null && b.startsWith(a), 
             d(b) + " starts with " + q(a), 
             d(b) + " doesn't start with " + q(a))
    }
  }
  /**
   * Matches if b.endsWith(a)
   */   
  def endWith[T <: String](t: =>T) = new Matcher[T] { 
    def apply(v: =>T)(d: Any => String) = {
      val (a, b) = (t, v)
      result(b!= null && a!= null && b.endsWith(a), 
             d(b) + " ends with " + q(a), 
             d(b) + " doesn't end with " + q(a))
    }
  }
  /**
   * Matches if the regexp a is found inside b
   */   
  def find[T <: String](a: T) = new FindMatcher(a)

  /**
   * Matcher to find if the regexp a is found inside b. 
   * This matcher can be specialized to a FindMatcherWithGroups which will also check the found groups
   */   
  class FindMatcher[T <: String](t: =>T) extends Matcher[T] {
    def found(a: T, b: T) = {
      val matcher = Pattern.compile(a).matcher(b)
      matcher.find
    }
    def withGroup(group: String) = new FindMatcherWithGroups(t, group)
    def withGroups(groups: String*) = new FindMatcherWithGroups(t, groups:_*)
    def apply(v: =>T)(d: Any => String) = {
      val (a, b) = (t, v)
      result(a != null && b != null && found(a, b), 
             q(a) + " is found in " + d(b), 
             q(a) + " isn't found in " + d(b))
      } 
  }

  /**
   * Matcher to find if the regexp a is found inside b. 
   * This matcher checks if the found groups are really the ones expected
   */   
  class FindMatcherWithGroups[T <: String](t: =>T, groups: String*) extends Matcher[T] {
    def found(a: T, b: T) = {
      val matcher = Pattern.compile(a).matcher(b)
      val groupsFound = new scala.collection.mutable.ListBuffer[String]()
      while (matcher.find) { groupsFound += matcher.group(1) }
      groupsFound.toList
    }
    def apply(v: =>T)(d: Any => String) = {
      val (a, b) = (t, v)
      val groupsFound = found(a, b)
      val withGroups = if (groups.size > 1) " with groups " else " with group "
      def foundText = {
        if (groupsFound.isEmpty) 
          ". Found nothing" 
        else 
           ". Found: " + q(groupsFound.mkString(", "))
      }
      val groupsToFind = if (groups == null) Nil else groups.toList
      result(a != null && b != null && groupsFound == groupsToFind, 
             q(a) + " is found in " + d(b) + withGroups + q(groupsToFind.mkString(", ")), 
             q(a) + " isn't found in " + d(b) + withGroups + q(groupsToFind.mkString(", ")) + foundText)
    } 
  }
  /**
   * Matches if the length is n
   */
  def haveLength(n: Int) = new Matcher[String](){
    def apply(v: =>String)(d: Any => String) = {
      val string = v
      result(string.length == n, d(string) + " has length " + n, d(string) + " doesn't have length " + n)
    }
  }
  def ==/(s: String) = be_==/(s)
  def equalToIgnoringCase[T <: String](a: T) = beEqualToIgnoringCase(a)
  def equalToIgnoringSpace[T <: String](a: T) = beEqualToIgnoringSpace(a)
  def equalIgnoringCaseTo[T <: String](a: T) = beEqualToIgnoringCase(a)
  def equalIgnoringSpaceTo[T <: String](a: T) = beEqualToIgnoringSpace(a)
}
class StringEmptyMatcher extends Matcher[String] {
  def apply(v: => String)(d: Any => String) = {
    val s = v
    result(s.isEmpty, d(s) + " is empty", d(s) + " is not empty")
  }
}
class BeEqualToIgnoringCase[T <: String](t: =>T) extends Matcher[T] { 
  def apply(v: => T)(d: Any => String) = {
	val (a, b) = (t, v)
	result(a != null && b != null && a.equalsIgnoreCase(b), 
           d(b) + " is equal ignoring case to " + q(a), 
           d(b) + " is not equal ignoring case to " + q(a))
  } 
}