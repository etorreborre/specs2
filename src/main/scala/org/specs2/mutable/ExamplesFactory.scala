package org.specs2
package mutable

import specification.{Example, DefaultExampleFactory, ExampleFactory}

trait ExamplesFactory extends specification.ExamplesFactory { this: FragmentsBuilder =>

  override implicit def exampleFactory: ExampleFactory = new MutableExampleFactory

  private[specs2] class MutableExampleFactory extends DefaultExampleFactory {
    override def newExample(e: Example): Example = addExample(e)
  }

}