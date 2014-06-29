package org.specs2
package specification
package dsl
package mutable

import execute.AsResult
import control.ImplicitParameters._
import org.specs2.specification.script.StepParser
import specification.core._
import specification.create.FragmentsFactory

trait GWT extends org.specs2.specification.dsl.GWT { outer: MutableFragmentBuilder with FragmentsFactory =>
  private val factory = fragmentFactory

  implicit class stringStep(s: String) {
    def example[T, R: AsResult](parser: StepParser[T])(f: T => R) =
      outer.example(s)(parser)(f)

    def step[T](parser: StepParser[T]): StepParserStep[T] =
      StepParserStep(s, parser)

    def action[T](parser: StepParser[T]): StepParserAction[T] =
      StepParserAction(s, parser)
  }

  def step[T](description: String)(parser: StepParser[T])(action: T => Any): Fragments =
    addFragments(super.step(parser)(action)(description).append(factory.break))

  def action[T](description: String)(parser: StepParser[T])(action1: T => Any): Fragments =
    addFragments(super.action(parser)(action1)(description).append(factory.break))

  def example[T, R : AsResult](description: String)(parser: StepParser[T])(action: T => R): Fragments =
    addFragments(super.example(parser)(action).apply(description).append(factory.break))

  case class StepParserExample[T](s: String, parser: StepParser[T]) {
    def in[R : AsResult](f: T => R) = >>(f)

    def >>[R : AsResult](f: T => R) =
      example(s)(parser)(f)
  }

  case class StepParserStep[T](s: String, parser: StepParser[T]) {
    def apply(f: T => Any) =
      step(s)(parser)(f)
  }

  case class StepParserAction[T](s: String, parser: StepParser[T]) {
    def apply(f: T => Any) =
      action(s)(parser)(f)
  }
}

trait GivenWhenThenSyntax { this: GWT =>
  def Given[T](description: String)(parser: StepParser[T])(action: T => Any): Fragments =
    step("Given "+description)(parser)(action)

  def When[T](description: String)(parser: StepParser[T])(action: T => Any): Fragments =
    step("When "+description)(parser)(action)

  def Then[T, R : AsResult](description: String)(parser: StepParser[T])(action: T => R): Fragments =
    example("Then "+description)(parser)(action)
}

trait GivenWhenAndThenSyntax { this: GWT =>
  def given[T](description: String)(parser: StepParser[T])(action: T => Any): Fragments =
    step("given "+description)(parser)(action)

  def when[T](description: String)(parser: StepParser[T])(action: T => Any): Fragments =
    step("when "+description)(parser)(action)

  def andThen[T, R : AsResult](description: String)(parser: StepParser[T])(action: T => R): Fragments =
    example("then "+description)(parser)(action)
}


