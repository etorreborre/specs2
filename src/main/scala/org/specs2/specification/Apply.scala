package org.specs2
package specification
import execute._

/**
 * Apply a Context to several fragments
 */
trait Apply extends Context {

  def apply(f: Fragment): Fragments = apply(FragmentsBuilder.fragments(f))
  def apply(fs: =>Fragments): Fragments = {
    fs.map { (f: Fragment) => f match {
        case e: Example => e.map((r: Result) => apply(r))
        case other      => other
      }
    }
  }
}
