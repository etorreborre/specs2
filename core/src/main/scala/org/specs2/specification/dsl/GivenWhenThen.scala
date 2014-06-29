package org.specs2
package specification
package dsl

import org.specs2.control.Status
import org.specs2.execute.{Error, AsResult}
import control.ImplicitParameters._
import core._
import create.FragmentsFactory
import script.StepParser

trait GivenWhenThen { outer: FragmentsFactory =>
  private val factory = fragmentFactory

  def step[T](extractor: StepParser[T])(action: T => Unit) = { description: String =>
    extractor.parse(description).fold(
    e => Fragments(factory.step(Error("failed to extract a value from "+description+": "+e.getMessage+e.getStackTrace.mkString("\n", "\n", "\n")))),
    { case (d, t) =>
      Fragments(
        factory.text(d),
         factory.step(action(t)))
    })
  }
/*
  def when(description: String)(action: =>Any) = step(s"when $description")(action)
  def when[T](description: String, extractor: String => Option[T])(action: T => Any) =
    step(s"when $description", extractor)(action)

  def step[T](extractor: String => Option[T])(action: T => Any): Fragment = {
    val extracted = extractor(description)
    addParagraph(description)
    addFragment(factory.step(extracted.map(action).getOrElse(throw new Exception("failed to extract a value from "+description))))
  }

  def step[T](description: String, extractor: String => Option[(String, T)])(action: T => Any)(implicit p: ImplicitParam): Fragment = {
    extractor(description).map { case (d, v) =>
      addParagraph(d)
      addFragment(factory.step(action(v)))
    }.getOrElse(throw new Exception("failed to extract a value from "+description))
  }

  def andThen[R : AsResult](description: String)(r: =>R): Fragment =
    addExample(s"then $description")(r)

  def addExample[R : AsResult](description: String)(r: =>R): Fragment = {
    addFragment(factory.example(description, r))
    addFragment(factory.break)
  }

  def andThen[R : AsResult, T](description: String, extractor: String => Option[T])(r: T => R): Fragment =
    addExample(s"then $description", extractor)(r)

  def addExample[R : AsResult, T](description: String, extractor: String => Option[T])(r: T => R): Fragment = {
    extractor(description).map { d =>
      addFragment(factory.example(description, r(d)))
    }.getOrElse(throw new Exception("failed to extract a value from "+description))
  }

  private def addParagraph(description: String) = {
    addFragment(factory.text(description))
    addFragment(factory.break)
    addFragment(factory.backtab)
  }
*/
}

