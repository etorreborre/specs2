package org.mockito.internal.invocation

import org.hamcrest.Matcher
import org.mockito.internal.matchers.{EqualsFunction1, EqualsFunction0, ArrayEquals, Equals}

import org.mockito.internal.util.collections.ArrayUtils

import java.util.ArrayList
import java.util.List
import org.mockito.internal.progress.ThreadSafeMockingProgress2
import scala.collection.{GenSetLike, GenSeqLike, JavaConversions}
import JavaConversions._

/*
 * Copyright (c) 2007 Mockito contributors
 * This program is made available under the terms of the MIT License.
 */

/**
 * by Szczepan Faber, created at: 3/31/12
 */
object ArgumentsProcessor {
  // expands array varArgs that are given by runtime (1, [a, b]) into true
  // varArgs (1, a, b);
  def expandVarArgs(isVarArgs: Boolean, args: Array[Object]) = {

    if (!isVarArgs || new ArrayUtils().isEmpty(args) || args(args.length - 1) != null && !args(args.length - 1).getClass().isArray()) {
      if (args == null) Array.ofDim[Object](0) else args
    } else {
      val nonVarArgsCount = args.length - 1
      var varArgs: Array[Object] = null
      if (args(nonVarArgsCount) == null) {
        // in case someone deliberately passed null varArg array
        varArgs = Array[Object](null)
      } else {
        varArgs = ArrayEquals.createObjectArray(args(nonVarArgsCount))
      }
      val varArgsCount = varArgs.length
      val newArgs = Array.ofDim[Object](nonVarArgsCount + varArgsCount)
      System.arraycopy(args, 0, newArgs, 0, nonVarArgsCount)
      System.arraycopy(varArgs, 0, newArgs, nonVarArgsCount, varArgsCount)
      newArgs
    }
  }

  def argumentsToMatchers(arguments: Array[Object]): List[Matcher[_]] = {
    var matchers: List[Matcher[_]] = new ArrayList[Matcher[_]](arguments.length)
    for (arg <- arguments) {
      if (arg != null && arg.getClass.isArray) matchers.add(new ArrayEquals(arg))
      else if (arg != null && arg.getClass.getName.startsWith("scala.collection.mutable.WrappedArray")) matchers.add(new Equals(arg))
      else if (arg.isInstanceOf[Function0[_]]) {
        // evaluate the byname parameter to collect the argument matchers
        // if an exception is thrown we keep the value to compare it with the actual one (see "with Any" in the MockitoSpec and issue 82)
        val value = try { arg.asInstanceOf[Function0[_]].apply() } catch { case e: Throwable => e }
        // during the evaluation of the value some matchers might have been created. pull them.
        val argumentsMatchers = ThreadSafeMockingProgress2.pullLocalizedMatchers
        // if there are no matchers at all being registered this means that
        // we are invoking the method, not verifying it
        // in that case add a new matcher corresponding to the argument (an equals matcher using the value)
        if (argumentsMatchers.isEmpty) matchers.add(new EqualsFunction0(value))
        else {
          // otherwise we add all the existing arguments matchers +
          // we reset the state of the argumentMatchersStorage
          matchers.addAll(argumentsMatchers.map(_.getActualMatcher))
          ThreadSafeMockingProgress2.reportMatchers(argumentsMatchers.map(_.getActualMatcher))
        }
      }
      else if (arg.isInstanceOf[org.specs2.matcher.Matcher[_]]) {
        matchers.add(new org.specs2.mock.HamcrestMatcherAdapter(arg.asInstanceOf[org.specs2.matcher.Matcher[_]]))
      }
      // special case for sequences and sets because they have an apply method making them instances of Function1
      // yet they define a useful equals method
      else if (arg.isInstanceOf[GenSeqLike[_,_]] || arg.isInstanceOf[GenSetLike[_,_]]) {
        matchers.add(new Equals(arg))
      }
      else if (arg.isInstanceOf[scala.runtime.AbstractFunction1[_,_]]) {
        matchers.add(new EqualsFunction1(arg))
      }
      else matchers.add(new Equals(arg))
    }
    matchers
  }

}

