package org.specs2
package matcher

import java.util.regex._
import text.Quote._

/**
 * The `StringMatchers` trait provides matchers which are applicable to String objects
 */
trait StringMatchers extends StringBaseMatchers with StringBeHaveMatchers

private[specs2]
trait StringBaseMatchers { outer =>
  
  /** matches if (a.equalsIgnoreCase(b)) */   
  def beEqualToIgnoreCase(a: String) = new BeEqualToIgnoreCase(a)
  /** matches if (a.equalsIgnoreCase(b)) */   
  def ==/(s: String) = be_==/(s)
  /** matches if (a.notEqualIgnoreCase(b)) */   
  def be_!=/(a: String) = notBeEqualToIgnoreCase(a)  
  /** matches if (a.equalsIgnoreCase(b)) */   
  def be_==/(a: String) = beEqualToIgnoreCase(a)  
  /** matches if (a.equalsIgnoreCase(b)) */   
  def equalIgnoreCase(a: String) = beEqualToIgnoreCase(a)
  /** matches if (a.equalsIgnoreCase(b)) */   
  def equalToIgnoreCase(a: String) = beEqualToIgnoreCase(a)
  /** matches if (a.equalsIgnoreCase(b)) */   
  def equalIgnoreCaseTo(a: String) = beEqualToIgnoreCase(a)
  /** matches if !(a.equalsIgnoreCase(b)) */   
  def !=/(s: String) = be_!=/(s)
  /** matches if !(a.equalsIgnoreCase(b)) */   
  def notBeEqualToIgnoreCase(a: String) = beEqualToIgnoreCase(a).not 
  /** matches if !(a.equalsIgnoreSpace(b)) */   
  def notBeEqualToIgnoreSpace(a: String) = beEqualToIgnoreSpace(a).not 
  /** matches if (a.trim == b.trim) */   
  def beEqualToIgnoreSpace(t: =>String) = new Matcher[String] { 
    def apply[S <: String](v: =>Expectable[S]) = {
      val (a, b) = (t, v) 
      result(a != null && b.value != null && a.trim == b.value.trim, 
             b.description + " is equal ignoring space to " + q(a), 
             b.description + " is not equal ignoring space to " + q(a), b)
      }
  }
  /** matches if (a.trim == b.trim) */   
  def equalIgnoreSpace(a: =>String) = beEqualToIgnoreSpace(a)
  /** matches if (a.trim == b.trim) */   
  def equalToIgnoreSpace(a: String) = beEqualToIgnoreSpace(a)
  /** matches if (a.trim == b.trim) */   
  def equalIgnoreSpaceTo(a: String) = beEqualToIgnoreSpace(a)

  /** matches if (b.indexOf(a) >= 0) */   
  def contain(t: =>String) = new Matcher[String] { 
    def apply[S <: String](v: =>Expectable[S]) = {
      val (a, b) = (t, v)
      result(a != null && b.value != null && b.value.indexOf(a) >= 0, 
             b.description + " contains " + q(a), 
             b.description + " doesn't contain " + q(a), b)
    } 
  }
  /** matches if !(b.indexOf(a) >= 0) */   
  def notContain(a: String) = contain(a).not 
  /** matches if b matches the regular expression a */   
  def beMatching(t: =>String) = new Matcher[String] {
    def apply[S <: String](v: =>Expectable[S]) = {
      val (a, b) = (t, v)
      result(b.value matches a, 
             b.description + " matches " + q(a), 
             b.description + " doesn't match " + q(a), b)
    }
  }
  /** matches if b doesn't match the regular expression a */   
  def notBeMatching(a: String) = beMatching(a).not
  /** matches if b.startsWith(a) */   
  def startWith(t: =>String) = new Matcher[String] { 
    def apply[S <: String](v: =>Expectable[S]) = {
      val (a, b) = (t, v)
      result(b.value!= null && a!= null && b.value.startsWith(a), 
             b.description + " starts with " + q(a), 
             b.description + " doesn't start with " + q(a), b)
    }
  }
  /** matches if !b.startsWith(a) */   
  def notStartWith(a: String) = startWith(a).not
  /** matches if b.endsWith(a) */   
  def endWith(t: =>String) = new Matcher[String] { 
    def apply[S <: String](v: =>Expectable[S]) = {
      val (a, b) = (t, v)
      result(b.value!= null && a!= null && b.value.endsWith(a), 
             b.description  + " ends with " + q(a), 
             b.description  + " doesn't end with " + q(a), b)
    }
  }
  /** matches if !b.endsWith(a) */   
  def notEndWith(a: String) = endWith(a).not
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
    def apply[S <: String](v: =>Expectable[S]) = {
      val (a, b) = (t, v)
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
      while (matcher.find) { groupsFound += matcher.group(1) }
      groupsFound.toList
    }
    def apply[S <: String](v: =>Expectable[S]) = {
      val (a, b) = (t, v)
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
  /** matches if the length is n */
  def haveLength(n: Int) = new Matcher[String](){
    def apply[S <: String](v: =>Expectable[S]) = {
      val string = v
      result(string.value.length == n, 
    		 string.description  + " has length " + n,
    		 string.description  + " doesn't have length " + n, string)
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
    def length(n: Int) = result(haveLength(n))
    def startWith(s: String) = result(outer.startWith(s))
    def endWith(s: String) = result(outer.endWith(s))
    def startingWith(s: String) = result(outer.startWith(s))
    def endingWith(s: String) = result(outer.endWith(s))
    def equalIgnoreSpace(s: String) = result(outer.equalIgnoreSpace(s))
    def equalIgnoreCase(s: String) = result(outer.equalIgnoreCase(s))
  }
}

class StringEmptyMatcher extends Matcher[String] {
  def apply[S <: String](v: =>Expectable[S]) = {
    val b = v
    result(b.value.isEmpty, b.description  + " is empty", b.description  + " is not empty", b)
  }
}
class BeEqualToIgnoreCase(t: =>String) extends Matcher[String] { 
  def apply[S <: String](v: =>Expectable[S]) = {
	val (a, b) = (t, v)
	result(a != null && b.value != null && a.equalsIgnoreCase(b.value), 
         b.description  + " is equal ignoring case to " + q(a), 
         b.description  + " is not equal ignoring case to " + q(a), b)
  } 
}