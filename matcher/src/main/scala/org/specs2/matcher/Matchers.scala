package org.specs2
package matcher

import control.LanguageFeatures

/**
 * Trait aggregating the most common specs2 matchers
 */
trait Matchers extends AnyMatchers
                  with BeHaveMatchers
                  with TraversableMatchers
                  with MapMatchers
                  with StringMatchers
                  with ExceptionMatchers
                  with NumericMatchers
                  with OptionMatchers
                  with EitherMatchers
                  with TryMatchers
                  with EventuallyMatchers
                  with FutureMatchers
                  with MatchersImplicits
                  with ValueChecks
                  with LanguageFeatures

trait Matchers1 extends
       AnyBaseMatchers
  with TraversableBaseMatchers
  with StringBaseMatchers
  with ExceptionBaseMatchers
  with NumericBaseMatchers
  with OptionBaseMatchers
  with EitherBaseMatchers
  with TryBaseMatchers
  with EventuallyMatchers
  with FutureBaseMatchers

object Matchers extends Matchers

trait MustMatchers extends Matchers with MustExpectations
object MustMatchers extends MustMatchers

private[specs2] trait MustMatchers1 extends Matchers1 with MustExpectations1
private[specs2] trait MustThrownMatchers1 extends Matchers1 with MustThrownExpectations1

trait ShouldMatchers extends Matchers with ShouldExpectations
object ShouldMatchers extends ShouldMatchers

trait MustThrownMatchers extends Matchers with MustThrownExpectations
object MustThrownMatchers extends MustThrownMatchers

trait ShouldThrownMatchers extends Matchers with ShouldThrownExpectations
object ShouldThrownMatchers extends ShouldThrownMatchers

