package org.specs2
package specification

import scala.quoted._
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
trait Snippets extends org.specs2.execute.Snippets { outer: S2StringContextCreation with FragmentsFactory =>
  private val factory = outer.fragmentFactory

  implicit inline def snippetIsInterpolatedFragment[T](inline snippet: Snippet[T]): Interpolated =
    ${Snippets.createInterpolatedFragment('{snippet}, '{outer.fragmentFactory})}
}

object Snippets:

  def createInterpolatedFragment[T](snippetExpr: Expr[Snippet[T]], factoryExpr: Expr[FragmentFactory])(
    using qctx: QuoteContext, t: Type[T]): Expr[Interpolated] =
      import qctx.reflect._
      '{ new Interpolated {
           private val expression = ${Expr(rootPosition.sourceCode)}
           private val snippet: Snippet[t.Underlying] = ${snippetExpr}
           private val factory = ${factoryExpr}
           private val start = PositionLocation(${Expr(rootPosition.sourceFile.jpath.toString)}, ${Expr(rootPosition.startLine)}, ${Expr(rootPosition.startColumn)})
           private val end = PositionLocation(${Expr(rootPosition.sourceFile.jpath.toString)}, ${Expr(rootPosition.endLine)}, ${Expr(rootPosition.endColumn)})

           def prepend(text: String): Fragments =
             Fragments(factory.text(text).setLocation(start)).append(snippetFragments(snippet, end, expression))

           def snippetFragments(snippet: Snippet[t.Underlying], location: Location, expression: String): Fragments =
             Fragments(
               Seq(factory.text(snippet.show(expression)).setLocation(location)) ++
                 resultFragments(snippet, location) ++
                 checkFragments(snippet, location):_*)
           def resultFragments(snippet: Snippet[t.Underlying], location: Location) =
             if snippet.showResult.isEmpty then
               Seq()
             else
               Seq(factory.text("\n"+snippet.showResult).setLocation(location))
           def checkFragments(snippet: Snippet[t.Underlying], location: Location) =
             if snippet.mustBeVerified then
               Seq(factory.step(snippet.verify.mapMessage("Snippet failure: "+_)).setLocation(location))
             else
               Seq()
         }
       }
