package org.specs2
package matcher
import AnyMatchers._
import java.util.regex._
import specification._
/**
 * The <code>StringMatchers</code> trait provides matchers which are applicable to String objects
 */
trait StringMatchers extends StringBaseMatchers with StringBeHaveMatchers
trait StringBaseMatchers { outer =>
  
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
    def apply[S <: String](v: =>Expectable[S]) = {
      val (a, b) = (t, v) 
      result(a != null && b.value != null && a.trim == b.value.trim, 
             b.description + " is equal ignoring space to " + q(a), 
             b.description + " is not equal ignoring space to " + q(a), b)
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
  def contain(t: =>String) = new Matcher[String] { 
    def apply[S <: String](v: =>Expectable[S]) = {
      val (a, b) = (t, v)
      result(a != null && b.value != null && b.value.indexOf(a) >= 0, 
             b.description + " includes " + q(a), 
             b.description + " doesn't include " + q(a), b)
    } 
  }

  /**
   * Matches if b matches the regular expression a
   */   
  def beMatching(t: =>String) = new Matcher[String] {
    def apply[S <: String](v: =>Expectable[S]) = {
      val (a, b) = (t, v)
      result(b.value matches a, 
             b.description + " matches " + q(a), 
             b.description + " doesn't match " + q(a), b)
    }
  }
  /**
   * Matches if b.startsWith(a)
   */   
  def startWith(t: =>String) = new Matcher[String] { 
    def apply[S <: String](v: =>Expectable[S]) = {
      val (a, b) = (t, v)
      result(b.value!= null && a!= null && b.value.startsWith(a), 
             b.description + " starts with " + q(a), 
             b.description + " doesn't start with " + q(a), b)
    }
  }
  /**
   * Matches if b.endsWith(a)
   */   
  def endWith(t: =>String) = new Matcher[String] { 
    def apply[S <: String](v: =>Expectable[S]) = {
      val (a, b) = (t, v)
      result(b.value!= null && a!= null && b.value.endsWith(a), 
             b.description  + " ends with " + q(a), 
             b.description  + " doesn't end with " + q(a), b)
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
  /**
   * Matches if the length is n
   */
  def haveLength(n: Int) = new Matcher[String](){
    def apply[S <: String](v: =>Expectable[S]) = {
      val string = v
      result(string.value.length == n, 
    		 string.description  + " has length " + n,
    		 string.description  + " doesn't have length " + n, string)
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
  def notContain(a: String) = contain(a).not 
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
trait StringBeHaveMatchers { outer: StringBaseMatchers =>
  implicit def toStringResultMatcher(result: MatchResult[String]) = new StringResultMatcher(result)
  class StringResultMatcher(result: MatchResult[String]) {
    def matching(s: String) = result.expectable.applyMatcher(beMatching(s))
    def startingWith(s: String) = result.expectable.applyMatcher(startWith(s))
  }
}

class StringEmptyMatcher extends Matcher[String] {
  def apply[S <: String](v: =>Expectable[S]) = {
    val b = v
    result(b.value.isEmpty, b.description  + " is empty", b.description  + " is not empty", b)
  }
}
class BeEqualToIgnoringCase(t: =>String) extends Matcher[String] { 
  def apply[S <: String](v: =>Expectable[S]) = {
	val (a, b) = (t, v)
	result(a != null && b.value != null && a.equalsIgnoreCase(b.value), 
         b.description  + " is equal ignoring case to " + q(a), 
         b.description  + " is not equal ignoring case to " + q(a), b)
  } 
}