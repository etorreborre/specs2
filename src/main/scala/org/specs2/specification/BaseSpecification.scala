package org.specs2
package specification
import main._

trait BaseSpecification extends ExamplesBuilder with PredefinedFragments with Main {
  val examples: Examples
} 
