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

/**
 * This class is duplicated to allow some side-effect to happen during the evaluation of byname arguments
 */
@SuppressWarnings(Array("unchecked"))
class MatchersBinder extends Serializable {
  def bindMatchers(argumentMatcherStorage: ArgumentMatcherStorage, invocation: Invocation): InvocationMatcher = {
    
    /** start of ugly hack */
    /**
     * this invokes argumentsToMatcher, which in turns add matchers to the arguments matchers storage
     *
     * if this is not called then verification might fail, arguing that the number of matchers is not equal to the number of arguments
     */
    invocation.toString
    /** end of ugly hack */

    val lastMatchers: List[Matcher[_]] = argumentMatcherStorage.pullMatchers
    validateMatchers(invocation, lastMatchers)
    val invocationWithMatchers: InvocationMatcher = new InvocationMatcher(invocation, lastMatchers)
    invocationWithMatchers
  }

  private def validateMatchers(invocation: Invocation, matchers: List[Matcher[_]]) {
    if (!matchers.isEmpty) {
      val recordedMatchersSize: Int = matchers.size
      val expectedMatchersSize: Int = invocation.getArgumentsCount
      if (expectedMatchersSize != recordedMatchersSize) {
        new Reporter().invalidUseOfMatchers(expectedMatchersSize, recordedMatchersSize)
      }
    }
  }
}

@SuppressWarnings(Array("unchecked"))
object MatchersBinder {
  val serialVersionUID: Long = -311433939339443463L
}

