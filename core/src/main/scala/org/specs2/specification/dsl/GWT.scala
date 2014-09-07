package org.specs2
package specification
package dsl

import org.specs2.execute.{Error, AsResult}
import core._
import create.FragmentsFactory
import script.StepParser

/**
 * Given-When-Then Dsl for mutable specifications
 */
trait GWT { outer: FragmentsFactory =>
  private val factory = fragmentFactory

  /** create an action from a step parser */
  def action[T](parser: StepParser[T])(action1: T => Any) = { description: String =>
    parser.parse(description).fold(
    e => Fragments(factory.step(Error("failed to extract a value from "+description+": "+e.getMessage+e.getStackTrace.mkString("\n", "\n", "\n")))),
    { case (d, t) =>
      Fragments(
        factory.text(d),
        factory.action(action1(t)))
    })
  }

  /** create an step from a step parser */
  def step[T](parser: StepParser[T])(action: T => Any) = { description: String =>
    parser.parse(description).fold(
    e => Fragments(factory.step(Error("failed to extract a value from "+description+": "+e.getMessage+e.getStackTrace.mkString("\n", "\n", "\n")))),
    { case (d, t) =>
      Fragments(
        factory.text(d),
         factory.step(action(t)))
    })
  }

  /** create an example from a step parser */
  def example[T, R : AsResult](parser: StepParser[T])(action: T => R) = { description: String =>
    parser.parse(description).fold(
    e => Fragments(factory.step(Error("failed to extract a value from "+description+": "+e.getMessage+e.getStackTrace.mkString("\n", "\n", "\n")))),
    { case (d, t) =>
      Fragments(factory.example(d, action(t)))
    })
  }

}

