package org.specs2
package matcher

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
                  with MatchersImplicits

object Matchers extends Matchers

trait MustMatchers extends Matchers with MustExpectations
trait ShouldMatchers extends Matchers with ShouldExpectations

trait MustThrownMatchers extends Matchers with MustThrownExpectations
trait ShouldThrownMatchers extends Matchers with ShouldThrownExpectations
