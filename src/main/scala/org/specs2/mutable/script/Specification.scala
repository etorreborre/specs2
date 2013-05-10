package org.specs2.mutable.script

/**
 * This Specification trait is using a Script (`GroupScript`) and a default template `BulletedExamplesTemplate`
 * to associate extracted examples text (where there are `+` signs) to example bodies coming from a `Group`.
 * tod
 */
abstract class Specification extends SpecificationLike

/**
 * Trait for the mutable.script.Specification abstract class
 */
trait SpecificationLike extends org.specs2.specification.script.SpecificationLike with org.specs2.mutable.SpecificationFeatures {
  def is = fragments
}
