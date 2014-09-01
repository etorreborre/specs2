package org.specs2
package matcher

import text.Sentences
import execute.{ResultExecution, AsResult}

trait ExpectationsDescription extends Sentences with ExpectationsCreation {

  implicit def describeExpectation(description: String): ExpectationDescription = new ExpectationDescription(description)

  class ExpectationDescription(description: String) {
    def ==>[T : AsResult](result: =>T) = <==>(result)
    def <==>[T : AsResult](result: =>T) = checkResultFailure {
      val r = ResultExecution.execute(AsResult(result))
      r match {
        case i if i.isError || i.isFailure => i.mapMessage(m => negateSentence(description)+" because "+m)
        case other                         => other.mapMessage(m => description+" <=> "+m)
      }
    }
  }
}

object ExpectationsDescription extends ExpectationsDescription

trait NoExpectationsDescription extends ExpectationsDescription {
  override def describeExpectation(description: String): ExpectationDescription = super.describeExpectation(description)
}

