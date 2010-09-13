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
  def beEqualToIgnoringCase(a: String) = new BeEqualToIgnoringCase(a)
  /**
   * Matches if (a.equalsIgnoreCase(b))
   */   
  def be_==/(a: String) = beEqualToIgnoringCase(a)  
  /**
   * Matches if (a.trim == b.trim)
   */   
  def beEqualToIgnoringSpace(t: =>String) = new Matcher[String] { 
    def apply(v: => String)(d: =>String) = {
      val (a, b) = (t, v) 
      result(a != null && b != null && a.trim == b.trim, 
             d + " is equal ignoring space to " + q(a), 
             d + " is not equal ignoring space to " + q(a))
      }
  }
  /**
   * Matches if (a.trim == b.trim)
   * @deprecated use beEqualToIgnoringSpace instead
   */   
  def equalIgnoreSpace(a: =>String) = beEqualToIgnoringSpace(a)
  /**
   * Matches if (b.indexOf(a) >= 0)
   */   
  def include(t: =>String) = new Matcher[String] { 
    def apply(v: => String)(d: =>String) = {
      val (a, b) = (t, v)
      result(a != null && b != null && b.indexOf(a) >= 0, 
             d + " includes " + q(a), 
             d + " doesn't include " + q(a))
    } 
  }

  /**
   * Matches if b matches the regular expression a
   */   
  def beMatching(t: =>String) = new Matcher[String] {
    def apply(v: =>String)(d: =>String) = {
      val (a, b) = (t, v)
      result(a matches b, 
             d + " matches " + q(a), 
             d + " doesn't match " + q(a))
    }
  }
  /**
   * Matches if b.startsWith(a)
   */   
  def startWith(t: =>String) = new Matcher[String] { 
    def apply(v: =>String)(d: =>String) = {
      val (a, b) = (t, v)
      result(b!= null && a!= null && b.startsWith(a), 
             d + " starts with " + q(a), 
             d + " doesn't start with " + q(a))
    }
  }
  /**
   * Matches if b.endsWith(a)
   */   
  def endWith(t: =>String) = new Matcher[String] { 
    def apply(v: =>String)(d: =>String) = {
      val (a, b) = (t, v)
      result(b!= null && a!= null && b.endsWith(a), 
             d + " ends with " + q(a), 
             d + " doesn't end with " + q(a))
    }
  }
  /**
   * Matches if the regexp a is found inside b
   */   
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
    def apply(v: =>String)(d: =>String) = {
      val (a, b) = (t, v)
      result(a != null && b != null && found(a, b), 
             q(a) + " is found in " + d, 
             q(a) + " isn't found in " + d)
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
      while (matcher.find) { groupsFound += matcher.group(1) }
      groupsFound.toList
    }
    def apply(v: =>String)(d: =>String) = {
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
             q(a) + " is found in " + d + withGroups + q(groupsToFind.mkString(", ")), 
             q(a) + " isn't found in " + d + withGroups + q(groupsToFind.mkString(", ")) + foundText)
    } 
  }
  /**
   * Matches if the length is n
   */
  def haveLength(n: Int) = new Matcher[String](){
    def apply(v: =>String)(d: =>String) = {
      val string = v
      result(string.length == n, d + " has length " + n, d + " doesn't have length " + n)
    }
  }
  /**
   * Matches if (a.notEqualIgnoreCase(b))
   */   
  def be_!=/(a: String) = notEqualIgnoreCase(a)  

  /**
   * Matches if !(a.equalsIgnoreCase(b))
   * @deprecated use notBeEqualToIgnoringCase instead
   */   
  def notEqualIgnoreCase(a: String) = beEqualToIgnoringCase(a).not 
  /**
   * Matches if !(a.equalsIgnoreCase(b))
   */   
  def notBeEqualToIgnoringCase(a: String) = beEqualToIgnoringCase(a).not 
  /**
   * Matches if !(a.equalsIgnoreSpace(b))
   */   
  def notBeEqualToIgnoringSpace(a: String) = beEqualToIgnoringSpace(a).not 
  /**
   * Matches if !(b.indexOf(a) >= 0)
   */   
  def notInclude(a: String) = include(a).not 
  /**
   * Matches if b doesn't match the regular expression a
   */   
  def notBeMatching(a: String) = beMatching(a).not
  /**
   * Matches if !b.startsWith(a)
   */   
  def notStartWith(a: String) = startWith(a).not
  /**
   * Matches if !b.endsWith(a)
   */   
  def notEndWith(a: String) = endWith(a).not
  def !=/(s: String) = be_!=/(s)

  def ==/(s: String) = be_==/(s)
  def equalToIgnoringCase(a: String) = beEqualToIgnoringCase(a)
  def equalToIgnoringSpace(a: String) = beEqualToIgnoringSpace(a)
  def equalIgnoringCaseTo(a: String) = beEqualToIgnoringCase(a)
  def equalIgnoringSpaceTo(a: String) = beEqualToIgnoringSpace(a)
}
class StringEmptyMatcher extends Matcher[String] {
  def apply(v: => String)(d: =>String) = {
    val s = v
    result(s.isEmpty, d + " is empty", d + " is not empty")
  }
}
class BeEqualToIgnoringCase(t: =>String) extends Matcher[String] { 
  def apply(v: => String)(d: =>String) = {
	val (a, b) = (t, v)
	result(a != null && b != null && a.equalsIgnoreCase(b), 
           d + " is equal ignoring case to " + q(a), 
           d + " is not equal ignoring case to " + q(a))
  } 
}