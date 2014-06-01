package org.specs2
package specification
package core
package mutable

import specification.dsl.mutable.MutableFragmentBuilder

trait SpecificationStructure extends specification.core.SpecificationStructure
  with MutableFragmentBuilder {
  override def structure = super[MutableFragmentBuilder].structure
  override def fragments = super[MutableFragmentBuilder].fragments

  def is = {
    val env = Env() // transient env
    try structure(Env())
    finally env.shutdown
  }
}

