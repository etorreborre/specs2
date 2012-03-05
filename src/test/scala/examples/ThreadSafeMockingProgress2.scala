package org.mockito.internal.progress

object ThreadSafeMockingProgress2 extends ThreadSafeMockingProgress {
  def pullMatchers = ThreadSafeMockingProgress.threadSafely.getArgumentMatcherStorage.pullMatchers
}


