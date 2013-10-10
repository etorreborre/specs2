package org.specs2
package matcher

/**
 * These traits and objects can be used in JUnit
 */
trait JUnitMustMatchers extends Matchers with JUnitMustExpectations
object JUnitMustMatchers extends JUnitMustMatchers

trait JUnitShouldMatchers extends Matchers with JUnitShouldExpectations
object JUnitShouldMatchers extends JUnitShouldMatchers
