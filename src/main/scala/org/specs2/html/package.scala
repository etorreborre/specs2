package org.specs2
/**
 * This package provides newtypes for strings representing toc ids
 */
package object html extends data.TaggedTypes {

  type SpecId = Newtype[String, SpecIdOps]
  def SpecId(id: String): SpecId = newtype(id)
}

package html {

  /**
   * Wrapper for a specification id
   */
  case class SpecIdOps(s: String)
}
