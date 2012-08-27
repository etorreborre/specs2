package org.specs2
package specification

/**
* This trait deactivates the implicit on Strings which is used to create examples with !
*
* This is useful to avoid conflict with DataTables
*/
trait NoBangExamples extends FragmentsBuilder {
  override def forExample(desc: String): ExampleDesc = super.forExample(desc)
}