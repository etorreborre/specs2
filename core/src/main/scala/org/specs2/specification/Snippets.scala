package org.specs2
package specification

import org.specs2.execute._
import specification.core._
import specification.create._

/**
 * Snippets of code can be extracted from interpolated specification strings.
 *
 * When you want to specify that a piece of code must be included in the specification output, you can use the `snippet`
 * method to execute a this code and use the text in the output. If you just want to output part of the code you need to
 * delimit it with some comments `// 8<-------` (with as many dashes as you want)
 *
 * Generally the last value of a snippet will be displayed separately but it is possible to avoid this by using the `mute`
 * method on a Snippet.
 *
 * It is also possible to check that the result value is equal to a specific value by using the `check[R : AsResult](f: T => R)` method.
 *
 */
trait Snippets extends org.specs2.execute.Snippets { outer: S2StringContext with FragmentsFactory =>
  private val factory = outer.fragmentFactory

  implicit def snippetIsSpecPart[T](snippet: Snippet[T]): InterpolatedPart = new InterpolatedPart {
    def append(parts: Vector[Fragment], text: String, start: Location, end: Location, expression: String): Vector[Fragment] =
      (parts :+ factory.text(text).setLocation(start)) ++ snippetFragments(snippet, end, expression).fragments

    private def snippetFragments(snippet: Snippet[T], location: Location, expression: String): Fragments = {
      Fragments(
        Seq(factory.text(snippet.show(expression)).setLocation(location)) ++
          resultFragments(snippet, location) ++
          checkFragments(snippet, location):_*)
    }

    private def resultFragments(snippet: Snippet[T], location: Location) = {
      if (snippet.showResult.isEmpty) Seq()
      else                            Seq(factory.text("\n"+snippet.showResult).setLocation(location))
    }

    private def checkFragments(snippet: Snippet[T], location: Location) = {
      if (snippet.mustBeVerified) Seq(factory.step(snippet.verify.mapMessage("Snippet failure: "+_)).setLocation(location))
      else                        Seq()
    }

  }
}
