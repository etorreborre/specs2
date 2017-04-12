package org.specs2
package specification
package dsl

/**
 * Dsl for creating acceptance specifications
 */
trait AcceptanceDsl extends
       FragmentsDsl
  with SpecStructureDsl
  with TitleDsl
  with ExampleDsl
  with ReferenceDsl
  with TagDsl
  with ActionDsl


/**
 * Lightweight Dsl trait with just a few implicits:
 *
 *  - use arguments
 *  - use s2 string directly in "def is = ..."
 */
private[specs2]
trait AcceptanceDsl1 extends 
       SpecStructureDsl1 
  with ReferenceCreation
  with TagDsl
  with ActionDsl

private[specs2]
object AcceptanceDsl1 extends AcceptanceDsl1