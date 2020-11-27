package org.specs2
package specification
package create

import execute.AsResult
import text.Trim._
import core.{Description, Fragment, Fragments}
import scala.quoted._

/**
 * This trait allows to create examples where the description is the code itself
 * and the code returns an AsResult value
 */
trait AutoExamples extends FragmentsFactory:
  inline def eg[T](inline code: =>T)(using asResult: AsResult[T]): Fragments =
    ${ AutoExamples.create[T]('{() => code}, 'asResult, 'postProcessAutoExample) }

  /** this function is introduced just to allow the mutable specification to register the newly created fragments */
  def postProcessAutoExample(fs: Fragments): Fragments =
    fs

object AutoExamples extends AutoExamples:

  def create[T](code: Expr[() => T], asResult: Expr[AsResult[T]], postProcess: Expr[Fragments => Fragments])(
    using qctx: Quotes)(using t: Type[T], t1: Type[() => T]): Expr[Fragments] =

    import qctx.reflect._
    val expression = Expr(Position.ofMacroExpansion.sourceCode)
    Expr.betaReduce('{$postProcess(createExample[t.Underlying]($expression, $code, $asResult))})

  def createExample[T](expression: String, code: () => T, asResult: AsResult[T]): Fragments =
    Fragments(AutoExamples.makeExample(expression, code(), asResult))

  def makeExample[T](expression: String, code: =>T, asResult: AsResult[T]): Fragment =
    fragmentFactory.example(Description.code(trimExpression(expression)), code)(asResult)

  private[specs2] def trimExpression(call: String) =
    call.
      trimEnclosing("${", "}").
      trimStart("eg").
      trimEnclosing("{", "}").
      trimEnclosing("`", "`").
      removeFirst("`\\(.*\\)").trimFirst("`").split("\n", -1).map(_.trim).mkString("\n")
