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
  with LinkDsl
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
  with LinkCreation 
  with TagDsl
  with ActionDsl