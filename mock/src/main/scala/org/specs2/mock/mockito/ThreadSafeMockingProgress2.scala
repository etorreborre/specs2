package org.mockito.internal.progress

/**
 * provide access to the locally stored matchers created by the `argThat` method when evaluating byname arguments
 */
object ThreadSafeMockingProgress2 extends ThreadSafeMockingProgress {
  def pullMatchers = ThreadSafeMockingProgress.threadSafely.getArgumentMatcherStorage.pullMatchers
}


