package org
package specs2
package mutable
package script

/**
 * This Specification trait is using a Script (`GroupScript`) and a default template `BulletedExamplesTemplate`
 * to associate extracted examples text (where there are `+` signs) to example bodies coming from a `Group`.
 * tod
 */
abstract class Specification extends SpecificationLike

/**
 * Trait for the mutable.script.Specification abstract class
 */
trait SpecificationLike extends specification.script.SpecificationLike with specification.SpecificationFeatures
