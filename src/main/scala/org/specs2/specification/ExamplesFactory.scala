package org.specs2
package specification


trait ExamplesFactory {
  implicit def exampleFactory: ExampleFactory = new DefaultExampleFactory
}