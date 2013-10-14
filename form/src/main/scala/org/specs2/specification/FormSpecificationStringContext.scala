package org.specs2
package specification

import form._
import main._

/**
 * Allow to use forms inside interpolated strings starting with s2 in order to build the specification content
 */
trait FormSpecificationStringContext extends SpecificationStringContext { this: FormFragmentsBuilder with ArgumentsArgs with FormattingTags =>
  implicit def formIsSpecPart(f: =>Form): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") = fs.append(createTextFragment(text)).append(formsAreExamples(f.executeForm))
  }
  implicit def toFormIsSpecPart(f: { def form: Form}): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") = fs append { formIsSpecPart(f.form).append(text, expression) }
  }
}

