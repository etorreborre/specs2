package org.specs2
package specification
package core
package mutable

import specification.dsl.mutable.MutableFragmentBuilder

/**
 * Structure of a mutable specification
 */
private[specs2]
trait SpecificationStructure extends specification.core.SpecificationStructure
  with MutableFragmentBuilder

