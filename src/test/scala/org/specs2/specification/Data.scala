package org.specs2
package specification

import org.scalacheck.Gen

trait DataSpecificationStructure[+T] extends SpecificationStructure
trait DataSpecification[+T] extends Specification with DataSpecificationStructure[T]

trait Data[+T] {
  def sizeOf1[T](gen: Int => Gen[T]) = Gen.sized { size =>
    if (size <= 0) gen(1)
    else           gen(size)
  }
}
