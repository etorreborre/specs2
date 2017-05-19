package org.mockito.internal.progress

import org.mockito.ArgumentMatcher
import scala.collection.JavaConverters._

/**
 * provide access to the locally stored matchers created by the `argThat` method when evaluating byname arguments
 */
object ThreadSafeMockingProgress2 {
  def pullLocalizedMatchers: java.util.List[ArgumentMatcher[_]] =
    ThreadSafeMockingProgress.mockingProgress.getArgumentMatcherStorage.pullLocalizedMatchers().asScala.map(_.getMatcher).asJava

  def reportMatchers(matchers: java.util.List[ArgumentMatcher[_]]) = {
    matchers.asScala.foreach(m => ThreadSafeMockingProgress.mockingProgress.getArgumentMatcherStorage.reportMatcher(m))
  }
}


