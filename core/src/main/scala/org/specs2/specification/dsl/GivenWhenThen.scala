package org.specs2
package specification
package dsl

import org.specs2.execute.{Error, AsResult}
import core._
import create.FragmentsFactory
import script.StepParser

trait GivenWhenThen { outer: FragmentsFactory =>
  private val factory = fragmentFactory

  def action[T](extractor: StepParser[T])(action1: T => Any) = { description: String =>
    extractor.parse(description).fold(
    e => Fragments(factory.step(Error("failed to extract a value from "+description+": "+e.getMessage+e.getStackTrace.mkString("\n", "\n", "\n")))),
    { case (d, t) =>
      Fragments(
        factory.text(d),
        factory.action(action1(t)))
    })
  }

  def step[T](extractor: StepParser[T])(action: T => Any) = { description: String =>
    extractor.parse(description).fold(
    e => Fragments(factory.step(Error("failed to extract a value from "+description+": "+e.getMessage+e.getStackTrace.mkString("\n", "\n", "\n")))),
    { case (d, t) =>
      Fragments(
        factory.text(d),
         factory.step(action(t)))
    })
  }

  def example[T, R : AsResult](extractor: StepParser[T])(action: T => R) = { description: String =>
    extractor.parse(description).fold(
    e => Fragments(factory.step(Error("failed to extract a value from "+description+": "+e.getMessage+e.getStackTrace.mkString("\n", "\n", "\n")))),
    { case (d, t) =>
      Fragments(factory.example(d, action(t)))
    })
  }

}

