package org.specs2
package specification

import form.Form
import main.{Arguments, ArgumentsArgs}
import execute._
import text.NotNullStrings._

/**
 * Allow to use fragments inside interpolated strings starting with s2 in order to build the specification content
 */
trait SpecificationStringContext { outer: FragmentsBuilder with ArgumentsArgs =>

  implicit def exampleIsSpecPart(e: Example): SpecPart = new SpecPart {
    def appendTo(text: String) = text ^ e
  }

  implicit def asResultIsSpecPart[R : AsResult](r: =>R): SpecPart = new SpecPart {
    def appendTo(text: String) = {
      val texts = text.split("\n")
      val first = texts.dropRight(1).mkString("", "\n", "\n")
      AsResult(r) match {
        case DecoratedResult(t, e: Error) => first ^ texts.last ! e
        case DecoratedResult(t, _)        => textStart(text + t.notNull)
        case other                        => first ^ texts.last ! other
      }
    }
  }
  implicit def anyAsResultIsSpecPart(r: =>Function0Result): SpecPart = new SpecPart {
    def appendTo(text: String) = {
      val texts = text.split("\n")
      val first = texts.dropRight(1).mkString("", "\n", "\n")
      first ^ texts.last ! AsResult(r)
    }
  }
  implicit def formIsSpecPart(f: =>Form): SpecPart = new SpecPart {
    def appendTo(text: String) = text ^ Fragments.createList(Forms.formsAreExamples(f))
  }
  implicit def toFormIsSpecPart(f: { def form: Form}): SpecPart = new SpecPart {
    def appendTo(text: String) = text ^ Fragments.createList(Forms.formsAreExamples(f.form))
  }
  implicit def fragmentIsSpecPart(f: Fragment): SpecPart = new SpecPart {
    def appendTo(text: String) = text ^ f
  }
  implicit def fragmentsIsSpecPart(fs: Fragments): SpecPart = new SpecPart {
    def appendTo(text: String) = text ^ fs
  }
  implicit def argumentsIsSpecPart(a: Arguments): SpecPart = new SpecPart {
    def appendTo(text: String) = text ^ a
  }

  implicit class specificationInStringContext(sc: StringContext) {
    def s2(variables: SpecPart*) = outer.s2(sc, variables)
  }
  def s2(sc: StringContext, variables: Seq[SpecPart]) = {
    sc.parts.zip(variables).foldLeft(Fragments.createList() ^ args.report(noindent = true) ^ args.report(flow = true)) { (res, cur) =>
      val (text, extracted) = cur
      val appended = extracted.appendTo(text)

      // try to keep contiguous text fragments as one so that they can be properly rendered as Markdown
      // like numbered lists for example
      (res.middle, appended.middle) match {
        case (begin :+ Text(t1), Text(t2) +: rest) => (res ^ appended).copy(middle = begin ++ (Text(t1+t2) +: rest))
        case _                                     => res ^ appended
      }
    }
  }
}

trait SpecPart {
  def appendTo(text: String): Fragments
}

