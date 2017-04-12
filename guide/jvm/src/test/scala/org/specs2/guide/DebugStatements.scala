package org.specs2
package guide

object DebugStatements extends UserGuidePage { def is = s2"""

When quick and hacky `println` statements are what you want, the `Debug` trait, mixed in every `Specification`, provides useful methods:

 * `pp` or "print and pass", prints a value to the console, then returns it to be used in the rest of the expression: `graph.pp must haveSize(3)`
 * `pp(condition)` prints a value if a condition holds
 * `pp(f: T => Boolean)` prints a value if a condition on that value holds

"""
}
