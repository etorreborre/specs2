package org.specs2
/**
 * This package provides newtypes for strings representing toc ids
 */
package object html extends data.TaggedTypes {

  trait SpecIdentifier
  type SpecId = String @@ SpecIdentifier

  def specId(id: String): SpecId = tag(id)
}
