package org.mockito.internal.progress

/**
 * provide access to the matchers for arguments
 */
object ThreadSafeMockingProgress2 extends ThreadSafeMockingProgress {
  def pullMatchers = ThreadSafeMockingProgress.threadSafely.getArgumentMatcherStorage.pullMatchers
}


