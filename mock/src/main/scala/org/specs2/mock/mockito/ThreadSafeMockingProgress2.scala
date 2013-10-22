package org.mockito.internal.progress

import scala.collection.JavaConversions._
import org.hamcrest.Matcher
import org.mockito.internal.matchers.LocalizedMatcher

/**
 * provide access to the locally stored matchers created by the `argThat` method when evaluating byname arguments
 */
object ThreadSafeMockingProgress2 extends ThreadSafeMockingProgress {
  def pullLocalizedMatchers = ThreadSafeMockingProgress.threadSafely().getArgumentMatcherStorage.pullLocalizedMatchers()

  def reportMatchers(matchers: java.util.List[Matcher[_]]) = {
    matchers.foreach(m => ThreadSafeMockingProgress.threadSafely().getArgumentMatcherStorage.reportMatcher(m))
  }
}


