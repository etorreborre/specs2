package org.specs2
package specification.create

import execute._
import control.TraceLocation
import org.specs2.specification.script.StepParser
import specification.core._
import text.{Trim, Interpolated}
import Trim._
import text.NotNullStrings._
import text.Trim._

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
trait S2StringContext extends S2StringContext1 { outer =>

  implicit def descriptionToFragmentsIsInterpolatedFragment(fragments: String => Fragments): InterpolatedFragment = new InterpolatedFragment {
    def append(fs: Fragments, text: String, start: Location, end: Location, expression: String): Fragments = {
      val (description, before) = descriptionAndBefore(text, start, end, expression)
      fs append before append fragments(description.show)
    }
  }

  implicit def specificationRefIsInterpolatedFragment(ref: SpecificationRef): InterpolatedFragment = new InterpolatedFragment {
    def append(fs: Fragments, text: String, start: Location, end: Location, expression: String): Fragments = {
      fs append ff.text(text).setLocation(start) append fragmentFactory.link(ref).setLocation(end)
    }
  }

  implicit def stringFunctionIsInterpolatedFragment[R : AsResult](f: String => R): InterpolatedFragment =
    stringAndEnvFunctionIsInterpolatedFragment((s: String) => (e: Env) => f(s))

  implicit def stepParserIsInterpolatedFragment[R : AsResult](f: StepParser[R]): InterpolatedFragment =
    stringAndUpdatedDescriptionAndEnvFunctionIsInterpolatedFragment { s: String =>
      val parsed: Either[Throwable, (String, R)] = f.parse(s)
      (parsed.fold(_ => None, sr => Some(sr._1)), (e: Env) => parsed.fold(t => Error(t), sr => AsResult(sr._2)))
    }

  implicit def executionIsInterpolatedFragment(execution: Execution): InterpolatedFragment =
    createExecutionInterpolatedFragment(execution)

  implicit def anyAsResultIsInterpolatedFragment(r: =>Function0Result): InterpolatedFragment = new InterpolatedFragment {
    def append(fs: Fragments, text: String, start: Location, end: Location, expression: String): Fragments =
      asResultIsInterpolatedFragment(AsResult(r)).append(fs, text, start, end, expression)
  }

  implicit def specificationStructureIsInterpolatedFragment(s: SpecificationStructure): InterpolatedFragment = {
    lazy val specStructure = s.is
    fragmentIsInterpolatedFragment(ff.see(SpecificationRef(specStructure.header, specStructure.arguments, alias = specStructure.header.show)))
  }

  implicit def specStructureIsInterpolatedFragment(s: SpecStructure): InterpolatedFragment = new InterpolatedFragment {
    def append(fs: Fragments, text: String, start: Location, end: Location, expression: String): Fragments =
      (fs append ff.text(text).setLocation(start)) append s.fragments
  }

  implicit def stringIsInterpolatedFragment(s: =>String): InterpolatedFragment = new InterpolatedFragment {
    def append(fs: Fragments, text: String, start: Location, end: Location, expression: String): Fragments =  {
      val s1 =
        try s
        catch { case e: Throwable => s"[${e.getMessage.notNull}]" }
      fs append ff.text(text + s1).setLocation(start)
    }
  }

  implicit def fragmentsIsInterpolatedFragment(fragments: Fragments): InterpolatedFragment = new InterpolatedFragment {
    def append(fs: Fragments, text: String, start: Location, end: Location, expression: String): Fragments =
      (fs append ff.text(text).setLocation(start)) append fragments
  }

}

/**
 * Lightweight methods to interpolate fragments where only results and fragment can be interpolated
 */
private[specs2]
trait S2StringContext1 extends S2StringContextCreation { outer =>

  implicit def fragmentIsInterpolatedFragment(f: =>Fragment): InterpolatedFragment = new InterpolatedFragment {
    def append(fs: Fragments, text: String, start: Location, end: Location, expression: String): Fragments =
      fs append ff.text(text).setLocation(start) appendLazy f.setLocation(end)
  }

  implicit def asResultIsInterpolatedFragment[R : AsResult](r: =>R): InterpolatedFragment =
    stringAndEnvFunctionIsInterpolatedFragment(_ => (env: Env) => r)

}

/**
 * Methods to create interpolated fragments with no implicits
 */
trait S2StringContextCreation extends FragmentsFactory { outer =>
  private[specs2] val ff = fragmentFactory

  def createExecutionInterpolatedFragment(execution: Execution): InterpolatedFragment = new InterpolatedFragment {
    def append(fs: Fragments, text: String, start: Location, end: Location, expression: String) = {
      val (description, before) = descriptionAndBefore(text, start, end, expression)
      fs append before append ff.example(description, execution).setLocation(end)
    }
  }

  private[specs2] def stringAndEnvFunctionIsInterpolatedFragment[R : AsResult](f: String => Env => R): InterpolatedFragment =
    stringAndUpdatedDescriptionAndEnvFunctionIsInterpolatedFragment((s: String) => (None, (e: Env) => f(s)(e)))

  private[specs2] def stringAndUpdatedDescriptionAndEnvFunctionIsInterpolatedFragment[R : AsResult](f: String => (Option[String], Env => R)): InterpolatedFragment =
    new InterpolatedFragment {
    def append(fs: Fragments, text: String, start: Location, end: Location, expression: String) =  {
      val (description, before) = descriptionAndBefore(text, start, end, expression)

      val result =
        implicitly[AsResult[R]] match {
          case v : AnyValueAsResult[_] =>
            val (newDescription, executionFunction) = f(description.show)
            val (updatedDescription, r) = (newDescription.fold(description)(Text.apply), Env.executeResult(executionFunction))
            (r : @unchecked) match {
              case DecoratedResult(t, e: Error) => before :+ ff.example(updatedDescription, e).setLocation(end)
              case DecoratedResult(t, _)        => Vector(ff.text(text), ff.text(t.notNull)).map(_.setLocation(end))
            }

          case other =>
            val (newDescription, executionFunction) = f(description.show)
            val updatedDescription = newDescription.fold(description)(Text.apply)

            before :+ ff.example(updatedDescription, Execution.withEnv(executionFunction)).setLocation(end)
        }

      fs append result
    }
  }

  private[specs2] def descriptionAndBefore(text: String, start: Location, end: Location, expression: String): (Description, Vector[Fragment]) = {
    val texts = text.replace("\t", "  ").split("\n", -1).toSeq
    val autoExample = texts.lastOption.exists(_.trim.isEmpty)

    if (autoExample) {
      (Description.code(expression.removeEnclosing("`")),
        if (texts.size == 1) Vector()
        else                 Vector(ff.text(texts.mkString("\n")).setLocation(start)))
    }
    else {
      val lastLine = texts.lastOption.getOrElse("")
      val lastIndent = lastLine.takeWhile(_ == ' ')
      val (descriptionLines, beforeLines) = texts.reverse.span(_.takeWhile(_ == ' ') == lastIndent)

      val description =
        if (descriptionLines.size > 1)
          if (lastLine.trim.startsWith("|")) descriptionLines.reverse.map(_.removeFirst("\\|")).mkString("\n")
          else descriptionLines.reverse.mkString("\n")
        else descriptionLines.map(_.dropWhile(_ == ' ')).reverse.mkString("\n")

      val before =
        if (beforeLines.isEmpty) Vector()
        else                     Vector(ff.text(beforeLines.reverse.mkString("", "\n", "\n" + lastIndent)).setLocation(start))

      (Description.text(description.removeStart(lastIndent).trimEndSpace), before)
    }
  }

  /**
   * based on the interpolated variables and the expressions captured with the macro, create the appropriate fragments
   *
   * if the Yrangepos scalac option is not set then we use an approximated method to find the expressions texts
   */
  def s2(content: String,
         Yrangepos: Boolean,
         texts: Seq[String],
         textsStartPositions: Seq[String],
         textsEndPositions: Seq[String],
         variables: Seq[InterpolatedFragment],
         rangeExpressions: Seq[String]): Fragments =  {

    val expressions = if (Yrangepos) rangeExpressions else new Interpolated(content, texts).expressions

    val (textsStartLocations1, textsEndLocations1) = 
      (positionsToLocation(textsStartPositions), positionsToLocation(textsEndPositions))

    val fragments = (texts zip variables zip expressions zip textsStartLocations1 zip textsEndLocations1).foldLeft(Fragments()) { (res, cur) =>
      val ((((text, variable), expression), startLocation), endLocation) = cur
      variable.append(res, text, SimpleLocation(startLocation), SimpleLocation(endLocation), expression)
    }

    // The last piece of text is trimmed to allow the placement of closing quotes in the s2 string
    // to be on column 0 or aligned with examples and still have the same display when using the Text printer
    val last = texts.lastOption.map(_.trimEnd).filterNot(_.isEmpty).map(ff.text).toSeq

    fragments append Fragments(last:_*)
  }

  implicit class specificationInStringContext(sc: StringContext) {
    def s2(variables: InterpolatedFragment*): Fragments =
      macro S2Macro.s2Implementation
  }

  
  private def positionsToLocation(positions: Seq[String]): Seq[TraceLocation] =
    positions.map(_.split("\\|").toList).map {
      case path :: fileName :: line :: _ => TraceLocation(path, fileName, "Specification", "s2", line.toInt)
      // this case should not happen!
      case other                         => TraceLocation("not found", "file name", "Specification", "s2", 0)
    }
}

object S2StringContext extends DefaultFragmentFactory


/**
 * An interpolated fragment
 *
 *  - is appended to the previous fragments
 *  - can use the previous text, start location, end location and interpolated expression to create new Fragments
 */
trait InterpolatedFragment {
  def append(fragments: Fragments, text: String, start: Location, end: Location, expression: String): Fragments
}

