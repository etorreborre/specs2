package org.specs2
package specification

import execute._
import main._
import Snippet._

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
trait Snippets extends execute.Snippets { outer: SpecificationStringContext with FragmentsBuilder with ArgumentsArgs =>

  implicit def snippetIsSpecPart[T](snippet: Snippet[T]): SpecPart = new SpecPart {
    def appendTo(text: String, expression: String = "") =
      text ^ snippetFragments(snippet, expression)

    private def snippetFragments(snippet: Snippet[T], expression: String) = {
      snippet match {
        case cs: CodeSnippet[_]      => Fragments.createList(Text(snippet.markdown(expression))).add(resultFragments(snippet))
        case cc: CheckedSnippet[_,_] => Fragments.createList(Text(snippet.markdown(expression)), Step(cc.verify.mapMessage("Snippet failure: "+_)))
      }
    }

    private def resultFragments(snippet: Snippet[T]) = {
      if (snippet.resultMarkdown.isEmpty) Fragments.createList()
      else                                Fragments.createList(Text(snippet.resultMarkdown))
    }


  }
}

