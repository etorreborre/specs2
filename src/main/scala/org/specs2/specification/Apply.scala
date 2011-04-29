package org.specs2
package specification
import execute._

/**
 * Apply a Context to a sequence of fragments containing examples.
 *
 * The context is then applied to each example
 */
trait Apply extends Context {

  def apply(f: Fragment): Fragments = apply(FragmentsBuilder.fragments(f))
  def apply(fs: =>Fragments): Fragments = {
    fs.map { (f: Fragment) => f match {
        case e: Example => Example(e.desc, apply(e.execute))
        case other      => other
      }
    }
  }
}
/** apply a Before context to each Example */
trait BeforeEach extends Before with Apply
/** apply an After context to each Example */
trait AfterEach extends After with Apply
/** apply an Around context to each Example */
trait AroundEach extends Around with Apply
/** apply a BeforeAfter context to each Example */
trait BeforeAfterEach extends BeforeAfter with Apply
/** apply a BeforeAfterAround context to each Example */
trait BeforeAfterAroundEach extends BeforeAfterAround with Apply
