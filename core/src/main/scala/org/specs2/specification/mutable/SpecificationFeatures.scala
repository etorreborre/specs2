package org.specs2
package specification
package mutable

import matcher.{ShouldThrownMatchers, MustThrownMatchers}

/**
 * Mutable specification features
 */
trait SpecificationFeatures extends specification.SpecificationFeatures
  with MustThrownMatchers
  with ShouldThrownMatchers
