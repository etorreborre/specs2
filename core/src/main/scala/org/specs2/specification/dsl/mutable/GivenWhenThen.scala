package org.specs2
package specification
package dsl
package mutable

import execute.AsResult
import control.ImplicitParameters._
import org.specs2.specification.script.StepParser
import specification.core._
import specification.create.FragmentsFactory

trait GivenWhenThen extends org.specs2.specification.dsl.GivenWhenThen { outer: MutableFragmentBuilder with FragmentsFactory =>
  private val factory = fragmentFactory

  implicit class stringStep(s: String) {
    def example[T, R: AsResult](extractor: StepParser[T])(f: T => R) =
      outer.example(s)(extractor)(f)

    def step[T](extractor: StepParser[T]): StepParserStep[T] =
      StepParserStep(s, extractor)

    def action[T](extractor: StepParser[T]): StepParserAction[T] =
      StepParserAction(s, extractor)
  }

  def step[T](description: String)(extractor: StepParser[T])(action: T => Any): Fragments =
    addFragmentsBlock(super.step(extractor)(action)(description))

  def action[T](description: String)(extractor: StepParser[T])(action1: T => Any): Fragments =
    addFragmentsBlock(super.action(extractor)(action1)(description))

  def example[T, R : AsResult](description: String)(extractor: StepParser[T])(action: T => R): Fragments =
    addFragmentsBlock(super.example(extractor)(action).apply(description))

  case class StepParserExample[T](s: String, extractor: StepParser[T]) {
    def in[R : AsResult](f: T => R) = >>(f)

    def >>[R : AsResult](f: T => R) =
      example(s)(extractor)(f)
  }

  case class StepParserStep[T](s: String, extractor: StepParser[T]) {
    def apply(f: T => Any) =
      step(s)(extractor)(f)
  }

  case class StepParserAction[T](s: String, extractor: StepParser[T]) {
    def apply(f: T => Any) =
      action(s)(extractor)(f)
  }
}

trait GivenWhenThenSyntax { this: GivenWhenThen =>
  def Given[T](description: String)(extractor: StepParser[T])(action: T => Any): Fragments =
    step("Given "+description)(extractor)(action)

  def When[T](description: String)(extractor: StepParser[T])(action: T => Any): Fragments =
    step("When "+description)(extractor)(action)

  def Then[T, R : AsResult](description: String)(extractor: StepParser[T])(action: T => R): Fragments =
    example("Then "+description)(extractor)(action)
}

trait GivenWhenAndThenSyntax { this: GivenWhenThen =>
  def given[T](description: String)(extractor: StepParser[T])(action: T => Any): Fragments =
    step("given "+description)(extractor)(action)

  def when[T](description: String)(extractor: StepParser[T])(action: T => Any): Fragments =
    step("when "+description)(extractor)(action)

  def andThen[T, R : AsResult](description: String)(extractor: StepParser[T])(action: T => R): Fragments =
    example("then "+description)(extractor)(action)
}


