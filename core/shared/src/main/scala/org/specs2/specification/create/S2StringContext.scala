package org.specs2
package specification.create

import scala.quoted._
import execute._
import control.TraceLocation
import specification.script.StepParser
import specification.core._
import text.{Trim}
import Trim._
import text.NotNullStrings._
import text.Trim._
import S2StringContext._

/**
 * These implicit methods declare which kind of object can be interpolated in a s2 string;
 *
 *  - a function using the previous text and returning Fragments
 *  - specification references
 *  - examples using the Env, arguments, the statistics repository, the command line arguments
 *  - other specifications
 *  - strings
 *  - fragments
 *
 */
trait S2StringContext extends S2StringContext1 {

  /** create an example based on an execution.
   */
  implicit inline def asExecutionIsInterpolated[R : AsExecution](inline r: =>R)(implicit inline factory: FragmentFactory): Interpolated =
    ${executionInterpolated('{AsExecution[R].execute(r)}, 'factory)}

  implicit def specificationRefIsInterpolated(ref: SpecificationRef): Interpolated = new Interpolated {
    def append(text: String): Fragments =
      Fragments(fragmentFactory.text(text), fragmentFactory.link(ref))
  }

  implicit def specificationStructureIsInterpolated(s: SpecificationStructure): Interpolated = new Interpolated {
    val specStructure = s.is
    val ref = SpecificationRef(specStructure.header, specStructure.arguments, alias = specStructure.header.show)

    def append(text: String): Fragments =
      Fragments(fragmentFactory.text(text), fragmentFactory.see(ref))
  }

  implicit def specStructureIsInterpolated(s: SpecStructure): Interpolated = new Interpolated {
    def append(text: String): Fragments =
      Fragments(fragmentFactory.text(text)).append(s.fragments)
  }

  implicit def stringIsInterpolated(s: =>String): Interpolated = new Interpolated {
    def append(text: String): Fragments =  {
      val s1 =
        try s
        catch { case e: Throwable => s"[${e.getMessage.notNull}]" }
      Fragments(fragmentFactory.text(text + s1))
    }
  }

  implicit def fragmentsAreInterpolated(fragments: Fragments): Interpolated = new Interpolated {
    def append(text: String): Fragments =
      Fragments(fragmentFactory.text(text)).append(fragments)
  }

}

/**
 * Lightweight methods to interpolate fragments where only results and fragment can be interpolated
 */
private[specs2]
trait S2StringContext1 extends S2StringContextCreation {

  implicit inline def fragmentIsInterpolated(inline f: =>Fragment): Interpolated = new Interpolated {
    def append(text: String): Fragments =
      Fragments(fragmentFactory.text(text)).appendLazy(f)
  }

  implicit inline def asResultIsInterpolated[R : AsResult](inline r: =>R): Interpolated =
    ${executionInterpolated('{Execution.result(r)}, 'fragmentFactory)}
}

trait S2StringContextCreation extends FragmentsFactory {
  /** The FragmentFactory has to be passed as an implicit in order to be inlined in macros */
  implicit val _fragmentFactory: FragmentFactory = fragmentFactory

  /**
   * String interpolation for specs2 fragments
   */
  implicit class StringContextOps(sc: StringContext)(implicit factory: FragmentFactory) {
    inline def s2(inline variables: Interpolated*): Fragments =
      ${s2Implementation('sc)('variables, 'factory, 'postProcessS2Fragments)}
  }

  /** this function is exposed so that it can be overridden with side-effects when using s2 strings in mutable specs */
  def postProcessS2Fragments(fs: Fragments): Fragments =
    fs
}

object S2StringContext {

  /**
   * Macro implementation for s2 strings. It extracts the s2 texts + interpolated fragments
   */
  def s2Implementation(sc: Expr[StringContext])(
        variables: Expr[Seq[Interpolated]],
        ff: Expr[FragmentFactory],
        postProcess: Expr[Fragments => Fragments])(using qctx: QuoteContext) : Expr[Fragments] = {

    val args = variables match {
      case Varargs(args) => args
      case _ => report.throwError("Expected a statically known argument list", variables)
    }

    '{s2(${sc}.parts, ${variables}, ${ff}, ${postProcess})}
  }

  /**
   * Create fragments based on captured texts + "interpolated fragments"
   *
   * Depending on the interpolated element being added the preceding text can be left as it is or used as
   * a description (e.g. to create an Example)
   */
  def s2(texts: Seq[String], interpolated: Seq[Interpolated], ff: FragmentFactory, postProcess: Fragments => Fragments): Fragments =  {
    val fragments = Fragments.reduce(texts zip interpolated) { case (res, cur) =>
      val (text, variable) = cur
      res.append(variable.append(text))
    }

    // The last piece of text is trimmed to allow the placement of closing quotes in the s2 string
    // to be on column 0 or aligned with examples and still have the same display when using the Text printer
    val last = texts.lastOption.map(_.trimEnd).filterNot(_.isEmpty).map(ff.text).toSeq

    postProcess(fragments append Fragments(last:_*))
  }

  def executionInterpolated(execution: Expr[Execution], ff: Expr[FragmentFactory])(using qctx: QuoteContext): Expr[Interpolated] = {
    import qctx.tasty._
    '{
      new Interpolated {
        def append(text: String): Fragments = {
          createExample($ff,
            text,
            $execution,
            ${Expr(rootPosition.sourceCode)},
            ${Expr(PositionLocation(rootPosition.sourceFile.jpath.toString, rootPosition.startLine, rootPosition.startColumn))})
        }
      }
    }
  }

  def createInterpolatedExecution(execution: Execution, sourceCode: String, start: PositionLocation, ff: FragmentFactory): Interpolated =
    new Interpolated {
      def append(text: String): Fragments = {
        createExample(ff,
          text,
          execution,
          sourceCode,
          start)
      }
    }

  private[specs2] def fragmentInterpolated(fragment: Fragment, start: PositionLocation, ff: FragmentFactory): Interpolated =
    new Interpolated {
      def append(text: String): Fragments =
        Fragments(ff.text(text), fragment.setLocation(start))
    }

  /**
   * The text captured before an interpolated execution can be arbitrarily long. It is used
   *  to create an example description when an Execution is interpolated.
   *
   * We interpret it in the following way:
   *
   *  1. if there is a piece of text on 1 line then the text is the execution description
   *
   *  2. if there is some text on more than one line then
   *
   *      2.1 if the last line contains only spaces (followed by the execution line) then this is an auto-example
   *           which uses its own 'sourceCode' as a Description
   *
   *      2.2 otherwise we divide the text into 2 parts:
   *           - all the lines having the same indentation as the last line
   *           - all the lines before
   *          we use the first set to create the example description and the second set to create
   *          a Text Fragment
   */
  private[specs2] def createExample(
    ff: FragmentFactory,
    text: String,
    execution: Execution,
    sourceCode: String,
    start: PositionLocation): Fragments = {

    val texts = text.replace("\t", "  ").split("\n", -1).toSeq

    // we have an auto-example when the last line is empty
    val isAutoExample = texts.lastOption.exists(_.trim.isEmpty)

    if (isAutoExample) {
      val example = ff.example(Description.code(sourceCode.removeEnclosing("`")), execution).setLocation(start)
      if (texts.size == 1)
        Fragments(example)
      else
        Fragments(ff.text(texts.mkString("\n")), example)
    }
    else {
      val lastLine = texts.lastOption.getOrElse("")
      // indentation of the last line (spaces before the text)
      val lastIndent = lastLine.takeWhile(_ == ' ')

      // extract the lines before the description based on the last indentation
      // all lines having the same indentation as the last line are considered as being
      // part of the description
      val (descriptionLines, beforeLines) = texts.reverse.span(_.takeWhile(_ == ' ') == lastIndent)

      val description =
        if (descriptionLines.size > 1)
          if (lastLine.trim.startsWith("|")) descriptionLines.reverse.map(_.removeFirst("\\|")).mkString("\n")
          else descriptionLines.reverse.mkString("\n")
        else
          descriptionLines.map(_.dropWhile(_ == ' ')).reverse.mkString("\n")

      val example =
            ff.example(Description.text(description.removeStart(lastIndent).trimEndSpace), execution).setLocation(start)

      if (beforeLines.isEmpty)
        Fragments(example)
      else
        Fragments(ff.text(beforeLines.reverse.mkString("", "\n", "\n" + lastIndent)), example)
    }
  }

}
