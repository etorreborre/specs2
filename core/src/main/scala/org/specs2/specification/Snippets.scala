package org.specs2
package specification

import scala.quoted.*
import org.specs2.execute.*
import specification.core.*
import specification.create.*

/** Snippets of code can be extracted from interpolated specification strings.
  *
  * When you want to specify that a piece of code must be included in the specification output, you can use the
  * `snippet` method to execute a this code and use the text in the output. If you just want to output part of the code
  * you need to delimit it with some comments `// 8<-------` (with as many dashes as you want)
  *
  * Generally the last value of a snippet will be displayed separately but it is possible to avoid this by using the
  * `mute` method on a Snippet.
  *
  * It is also possible to check that the result value is equal to a specific value by using the `check[R : AsResult](f:
  * T => R)` method.
  */
trait Snippets extends org.specs2.execute.Snippets { outer: S2StringContextCreation & FragmentsFactory =>
  private val factory = outer.fragmentFactory

  implicit inline def snippetIsInterpolatedFragment[T](inline snippet: Snippet[T]): Interpolated =
    ${ Snippets.createInterpolatedFragment[T]('{ snippet }, '{ outer.fragmentFactory }) }
}

object Snippets:

  def createInterpolatedFragment[T](snippetExpr: Expr[Snippet[T]], factoryExpr: Expr[FragmentFactory])(using
      qctx: Quotes,
      t: Type[T]
  ): Expr[Interpolated] =
    import qctx.reflect.*
    '{
      new Interpolated {
        private val expression = ${
          Expr(Position.ofMacroExpansion.sourceCode.getOrElse("no source code found to interpolate a Snippet"))
        }
        private val snippet: Snippet[?] = ${ snippetExpr }
        private val factory = ${ factoryExpr }
        private val start = PositionLocation(
          ${ Expr(Position.ofMacroExpansion.sourceFile.jpath.toString) },
          ${ Expr(Position.ofMacroExpansion.startLine) },
          ${ Expr(Position.ofMacroExpansion.startColumn) }
        )
        private val end = PositionLocation(
          ${ Expr(Position.ofMacroExpansion.sourceFile.jpath.toString) },
          ${ Expr(Position.ofMacroExpansion.endLine) },
          ${ Expr(Position.ofMacroExpansion.endColumn) }
        )

        def prepend(text: String): Fragments =
          Fragments(factory.text(text).setLocation(start)).append(snippetFragments(snippet, end, expression))

        def snippetFragments[S](snippet: Snippet[S], location: Location, expression: String): Fragments =
          Fragments(
            Seq(factory.text(snippet.show(expression)).setLocation(location)) ++
              resultFragments(snippet, location) ++
              checkFragments(snippet, location)*
          )

        def resultFragments[S](snippet: Snippet[S], location: Location): Seq[Fragment] =
          if snippet.showResult.isEmpty then Seq()
          else Seq(factory.text("\n" + snippet.showResult).setLocation(location))

        def checkFragments[S](snippet: Snippet[S], location: Location): Seq[Fragment] =
          if snippet.mustBeVerified then
            Seq(factory.step(snippet.verify.updateMessage("Snippet failure: " + _)).setLocation(location))
          else Seq()
      }
    }
