package org.specs2
package matcher

import control.LanguageFeatures

/**
 * Trait aggregating all of specs2 matchers 
 */
trait Matchers extends AnyMatchers 
                  with TraversableMatchers
                  with MapMatchers
                  with StringMatchers
                  with XmlMatchers
                  with JsonMatchers
                  with BeHaveMatchers
                  with ExceptionMatchers
                  with NumericMatchers
                  with OptionMatchers
                  with EitherMatchers
                  with EventuallyMatchers
                  with FileMatchers
                  with ContentMatchers
                  with MatchersImplicits
                  with LanguageFeatures

object Matchers extends Matchers

trait MustMatchers extends Matchers with MustExpectations
object MustMatchers extends MustMatchers
trait ShouldMatchers extends Matchers with ShouldExpectations
object ShouldMatchers extends ShouldMatchers

trait MustThrownMatchers extends Matchers with MustThrownExpectations
object MustThrownMatchers extends MustThrownMatchers

trait ShouldThrownMatchers extends Matchers with ShouldThrownExpectations
object ShouldThrownMatchers extends ShouldThrownMatchers

