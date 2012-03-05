/*
 * Copyright (c) 2007 Mockito contributors
 * This program is made available under the terms of the MIT License.
 */
package org.mockito.internal.invocation

import org.hamcrest.Matcher
import org.mockito.exceptions.PrintableInvocation
import org.mockito.exceptions.Reporter
import org.mockito.internal.debugging.Location
import org.mockito.internal.exceptions.VerificationAwareInvocation
import org.mockito.internal.invocation.realmethod.RealMethod
import org.mockito.internal.reporting.PrintSettings
import org.mockito.internal.reporting.PrintingFriendlyInvocation
import org.mockito.internal.util.MockUtil
import org.mockito.internal.util.ObjectMethodsGuru
import org.mockito.internal.util.Primitives
import org.mockito.invocation.InvocationOnMock
import java.lang.reflect.Method
import java.util.ArrayList
import java.util.Arrays
import java.util.List
import org.mockito.internal.matchers.{EqualsFunction0, ArrayEquals, Equals, MatchersPrinter}
import org.mockito.internal.progress.ThreadSafeMockingProgress

/**
 * Method call on a mock object.
 * <p>
 * Contains sequence number which should be globally unique and is used for
 * verification in order.
 * <p>
 * Contains stack trace of invocation
 */
@SuppressWarnings(Array("unchecked")) object Invocation {
  def expandVarArgs(isVarArgs: Boolean, args: Array[AnyRef]): Array[AnyRef] = {
    if (!isVarArgs || args(args.length - 1) != null && !args(args.length - 1).getClass.isArray) {
      return if (args == null) new Array[AnyRef](0) else args
    }
    val nonVarArgsCount: Int = args.length - 1
    var varArgs: Array[AnyRef] = null
    if (args(nonVarArgsCount) == null) {
      varArgs = Array[AnyRef](null)
    }
    else {
      varArgs = ArrayEquals.createObjectArray(args(nonVarArgsCount))
    }
    val varArgsCount: Int = varArgs.length
    var newArgs: Array[AnyRef] = new Array[AnyRef](nonVarArgsCount + varArgsCount)
    System.arraycopy(args, 0, newArgs, 0, nonVarArgsCount)
    System.arraycopy(varArgs, 0, newArgs, nonVarArgsCount, varArgsCount)
    return newArgs
  }

  val serialVersionUID: Long = 8240069639250980199L
  final val MAX_LINE_LENGTH: Int = 45
}
import Invocation._
@SuppressWarnings(Array("unchecked")) class Invocation extends PrintableInvocation with InvocationOnMock with PrintingFriendlyInvocation with VerificationAwareInvocation {
  def this(mock: AnyRef, mockitoMethod: MockitoMethod, args: Array[AnyRef], sequenceNumber: Int, realMethod: RealMethod) {
    this()
    this.method = mockitoMethod
    this.mock = mock
    this.realMethod = realMethod
    this.arguments = expandVarArgs(mockitoMethod.isVarArgs, args)
    this.rawArguments = args
    this.sequenceNumber = sequenceNumber
    this.location = new Location
  }

  def getMock: AnyRef = {
    return mock
  }

  def getMethod: Method = {
    return method.getJavaMethod
  }

  def getArguments: Array[AnyRef] = {
    return arguments
  }

  def isVerified: Boolean = {
    return verified || isIgnoredForVerification
  }

  def getSequenceNumber: Int = {
    return sequenceNumber
  }

  override def equals(o: Any): Boolean = {
    if (o == null || !(o.getClass == this.getClass)) {
      return false
    }
    var other: Invocation = o.asInstanceOf[Invocation]
    (this.mock == other.mock) && (this.method == other.method) && this.equalArguments(other.arguments)
  }

  private def equalArguments(arguments: Array[AnyRef]): Boolean = {
    return Arrays.equals(arguments, this.arguments)
  }

  override def hashCode: Int = {
    return 1
  }

  override def toString: String = {
    return toString(argumentsToMatchers, new PrintSettings)
  }

  protected def toString(matchers: List[Matcher[_]], printSettings: PrintSettings): String = {
    var matchersPrinter: MatchersPrinter = new MatchersPrinter
    var method: String = qualifiedMethodName
    var invocation: String = method + matchersPrinter.getArgumentsLine(matchers, printSettings)
    if (printSettings.isMultiline || (!matchers.isEmpty && invocation.length > MAX_LINE_LENGTH)) {
      return method + matchersPrinter.getArgumentsBlock(matchers, printSettings)
    }
    else {
      return invocation
    }
  }

  private def qualifiedMethodName: String = {
    return new MockUtil().getMockName(mock) + "." + method.getName
  }

  protected def argumentsToMatchers: List[Matcher[_]] = {
    var matchers: List[Matcher[_]] = new ArrayList[Matcher[_]](arguments.length)
    for (arg <- arguments) {
      if (arg != null && arg.getClass.isArray) {
        matchers.add(new ArrayEquals(arg))
      }
      else if (arg.isInstanceOf[Function0[_]]) {
        val v = arg.asInstanceOf[Function0[_]].apply();
//        if (v != null)
//          matchers.add(new EqualsFunction0(arg))
//        else
          matchers.addAll(org.mockito.internal.progress.ThreadSafeMockingProgress2.pullMatchers)
      }
      else {
        matchers.add(new Equals(arg))
      }
    }
    return matchers
  }

  def isToString: Boolean = {
    return new ObjectMethodsGuru().isToString(getMethod)
  }

  def isValidException(throwable: Throwable): Boolean = {
    var exceptions: Array[Class[_]] = this.getMethod.getExceptionTypes
    var throwableClass: Class[_] = throwable.getClass
    for (exception <- exceptions) {
      if (exception.isAssignableFrom(throwableClass)) {
        return true
      }
    }
    return false
  }

  def isValidReturnType(clazz: Class[_]): Boolean = {
    if (method.getReturnType.isPrimitive) {
      return Primitives.primitiveTypeOf(clazz) eq method.getReturnType
    }
    else {
      return method.getReturnType.isAssignableFrom(clazz)
    }
  }

  def isVoid: Boolean = {
    return this.method.getReturnType eq Void.TYPE
  }

  def printMethodReturnType: String = {
    return method.getReturnType.getSimpleName
  }

  def getMethodName: String = {
    return method.getName
  }

  def returnsPrimitive: Boolean = {
    return method.getReturnType.isPrimitive
  }

  def getLocation: Location = {
    return location
  }

  def getArgumentsCount: Int = {
    return arguments.length
  }

  def getRawArguments: Array[AnyRef] = {
    return this.rawArguments
  }

  def callRealMethod: AnyRef = {
    if (isDeclaredOnInterface) {
      new Reporter().cannotCallRealMethodOnInterface
    }
    return realMethod.invoke(mock, rawArguments)
  }

  def isDeclaredOnInterface: Boolean = {
    return this.getMethod.getDeclaringClass.isInterface
  }

  def toString(printSettings: PrintSettings): String = {
    return toString(argumentsToMatchers, printSettings)
  }

  private[invocation] def markVerified: Unit = {
    this.verified = true
  }

  def markStubbed(stubInfo: StubInfo): Unit = {
    this.stubInfo = stubInfo
  }

  def ignoreForVerification: Unit = {
    isIgnoredForVerification = true
  }

  private final var sequenceNumber: Int = 0
  private final var mock: AnyRef = null
  private final var method: MockitoMethod = null
  private final var arguments: Array[AnyRef] = null
  private final var rawArguments: Array[AnyRef] = null
  private final var location: Location = null
  private var verified: Boolean = false
  private var isIgnoredForVerification: Boolean = false
  private[invocation] final var realMethod: RealMethod = null
  private var stubInfo: StubInfo = null
}

