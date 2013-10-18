package org.mockito.internal.progress

import scala.collection.JavaConversions._
import org.hamcrest.Matcher

/**
 * provide access to the locally stored matchers created by the `argThat` method when evaluating byname arguments
 */
object ThreadSafeMockingProgress2 extends ThreadSafeMockingProgress {
  def getMatchers = {
    val storage = ThreadSafeMockingProgress.threadSafely.getArgumentMatcherStorage
    val matchers = storage.pullMatchers
    matchers.foreach(storage.reportMatcher)
    matchers
  }

  def pullMatchers = ThreadSafeMockingProgress.threadSafely().getArgumentMatcherStorage.pullMatchers

  def reportMatchers(matchers: java.util.List[Matcher[_]]) = {
    matchers.foreach(ThreadSafeMockingProgress.threadSafely().getArgumentMatcherStorage.reportMatcher)
  }
}


