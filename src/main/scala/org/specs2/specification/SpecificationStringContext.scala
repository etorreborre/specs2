package org.specs2
package specification

import form.Form
import main.ArgumentsArgs

/**
 * Allow to use fragments inside interpolated strings starting with s2 in order to build the specification content
 */
trait SpecificationStringContext { this: FragmentsBuilder with ArgumentsArgs =>

  implicit class specificationContext(sc: StringContext) {
    def s2(variables: Any*) = {
      sc.parts.zip(variables).foldLeft(Fragments.createList() ^ args.report(noindent = true) ^ args.report(flow = true)) { (res, cur) =>
        val (text, extracted) = cur
        val fs = extracted match {
          case fs: Fragments        => fs
          case f: Fragment          => Fragments.createList(f)
          case f: Form              => Fragments.createList(Forms.formsAreExamples(f))
          case other                => Fragments.createList(Action(other))
        }
        res ^ text ^ fs
      }
    }
  }
}
