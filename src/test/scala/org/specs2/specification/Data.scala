package org.specs2
package specification

trait DataSpecificationStructure[+T] extends SpecificationStructure
trait DataSpecification[+T] extends DataSpecificationStructure[T] with Specification
trait Data[+T]
