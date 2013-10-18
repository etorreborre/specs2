package org.mockito.internal.invocation

/*
 * Copyright (c) 2007 Mockito contributors
 * This program is made available under the terms of the MIT License.
 */

import java.io.Serializable
import java.util.List
import org.hamcrest.Matcher
import org.mockito.exceptions.Reporter
import org.mockito.internal.progress.ArgumentMatcherStorage
import org.mockito.invocation.Invocation
import org.mockito.internal.matchers.LocalizedMatcher

/**
 * This class is duplicated to allow some side-effect to happen during the evaluation of byname arguments
 */
@SuppressWarnings(Array("unchecked"))
class MatchersBinder extends Serializable {
  def bindMatchers(argumentMatcherStorage: ArgumentMatcherStorage, invocation: Invocation): InvocationMatcher = {

    /** start of ugly hack */
    /**
     * invokes argumentsToMatcher, which in turns add matchers to the arguments matchers storage
     *
     * if this is not called then verification might fail, arguing that the number of matchers is not equal to the number of arguments
     */
    ArgumentsProcessor.argumentsToMatchers(invocation.getArguments)
    /** end of ugly hack */

    val lastMatchers = argumentMatcherStorage.pullLocalizedMatchers
    validateMatchers(invocation, lastMatchers)
    val invocationWithMatchers: InvocationMatcher = new InvocationMatcher(invocation, lastMatchers.asInstanceOf[List[Matcher[_]]])
    invocationWithMatchers
  }

  private def validateMatchers(invocation: Invocation, lastMatchers: List[LocalizedMatcher]) {
    if (!lastMatchers.isEmpty) {
      val recordedMatchersSize: Int = lastMatchers.size
      val expectedMatchersSize: Int = invocation.getArguments.length
      if (expectedMatchersSize != recordedMatchersSize) {
        new Reporter().invalidUseOfMatchers(expectedMatchersSize, lastMatchers)
      }
    }
  }
}

@SuppressWarnings(Array("unchecked"))
object MatchersBinder {
  val serialVersionUID: Long = -311433939339443463L
}

