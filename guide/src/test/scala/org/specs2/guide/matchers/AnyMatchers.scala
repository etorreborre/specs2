package org.specs2
package guide
package matchers

object AnyMatchers extends UserGuideCard {
  def title = "Any"
  def text = s2"""
These matchers can be used with any object, regardless of its type:

 * `beLike { case exp => result }` checks if an object is like a given pattern. `result` can be any expression using a matcher
 * `beLike { case exp => exp must beXXX }` checks if an object is like a given pattern, and verifies a condition
 * `beNull` checks if an object is null
 * `beAsNullAs` when 2 objects must be null at the same time if one of them is null
 * `beOneOf(a, b, c)` checks if an object is one of a given list
 * `haveClass` checks the class of an object
 * `haveSuperclass` checks if the class of an object as another class as one of its ancestors
 * `haveInterface` checks if an object is implementing a given interface
 * `beAssignableFrom` checks if a class is assignable from another
 * `beAnInstanceOf[T]` checks if an object is an instance of type `T`

#### With a typeclass

These matchers can be used with types having a specific typeclass instance:

 Matcher    | Typeclass                       | Description
 -------    | -------                         | -----
 `beEmpty`  | `org.specs2.collection.IsEmpty` | matches values which can be described as "empty": string, list, option,...
 `haveSize` | `org.specs2.collection.Sized`   | matches values which can have a "size": string, list, JSON,...

"""
}
