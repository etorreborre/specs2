package org.specs2
package guide
package matchers

object AnyMatchers extends UserGuideCard {
  def title = "Any"
  def text = s2"""
These matchers can be used with any object, regardless of its type:

 * `beLike { case exp => result }` check if an object is like a given pattern. `result` can be any expression using a matcher
 * `beLike { case exp => exp must beXXX }` check if an object is like a given pattern, and verifies a condition
 * `beNull` check if an object is null
 * `beAsNullAs` when 2 objects must be null at the same time if one of them is null
 * `beOneOf(a, b, c)` check if an object is one of a given list
 * `haveClass` check the class of an object
 * `haveSuperclass` check if the class of an object as another class as one of its ancestors
 * `haveInterface` check if an object is implementing a given interface
 * `beAssignableFrom` check if a class is assignable from another
 * `beAnInstanceOf[T]` check if an object is an instance of type `T`

"""
}
