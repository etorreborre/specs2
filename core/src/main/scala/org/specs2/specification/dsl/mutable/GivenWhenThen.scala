package org.specs2
package specification
package dsl
package mutable

import execute.AsResult
import control.ImplicitParameters._
import specification.core._
import specification.create.FragmentsFactory

trait GivenWhenThen { outer: MutableFragmentBuilder with FragmentsFactory =>
  private val factory = fragmentFactory

  def given(description: String)(action: =>Any) = step(s"given $description")(action)

  def given[T](description: String, extractor: String => Option[T])(action: T => Any) =
    step(s"given $description", extractor)(action)

  def given[T](description: String, extractor: String => Option[(String, T)])(action: T => Any)(implicit p: ImplicitParam) = {
    step(s"given $description", extractor)(action)(p)
  }

  def when(description: String)(action: =>Any) = step(s"when $description")(action)
  def when[T](description: String, extractor: String => Option[T])(action: T => Any) =
    step(s"when $description", extractor)(action)

  def step(description: String)(action: =>Any): Fragment = {
    addParagraph(description)
    factory.step(action)
  }

  def step[T](description: String, extractor: String => Option[T])(action: T => Any): Fragment = {
    val extracted = extractor(description)
    addParagraph(description)
    factory.step(extracted.map(action).getOrElse(throw new Exception("failed to extract a value from "+description)))
  }

  def step[T](description: String, extractor: String => Option[(String, T)])(action: T => Any)(implicit p: ImplicitParam): Fragment = {
    extractor(description).map { case (d, v) =>
      addParagraph(d)
      factory.step(action(v))
    }.getOrElse(throw new Exception("failed to extract a value from "+description))
  }

  def andThen[R : AsResult](description: String)(r: =>R): Fragment =
    addFragment(factory.example(description, r))

  def andThen[R : AsResult, T](description: String, extractor: String => Option[T])(r: T => R): Fragment = {
    extractor(description).map { d =>
      addFragment(factory.example(description, r(d)))
    }.getOrElse(throw new Exception("failed to extract a value from "+description))
  }

  private def addParagraph(description: String) = {
    addFragment(factory.text(description))
    addFragment(factory.break)
    addFragment(factory.backtab)
  }

}

