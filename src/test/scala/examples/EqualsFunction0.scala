/*
 * Copyright (c) 2007 Mockito contributors
 * This program is made available under the terms of the MIT License.
 */
package org.mockito.internal.matchers

import org.hamcrest.Description
import org.hamcrest.SelfDescribing
import org.mockito.ArgumentMatcher
import java.io.Serializable

object EqualsFunction0 {
  val serialVersionUID: Long = -3395637450058086891L
}
import EqualsFunction0._
class EqualsFunction0 extends ArgumentMatcher[AnyRef] with ContainsExtraTypeInformation with Serializable {
  def this(wanted: AnyRef) {
    this()
    this.wanted = wanted
  }

  def matches(actual: AnyRef): Boolean = {
    if (wanted.isInstanceOf[Function0[_]] && actual.isInstanceOf[Function0[_]])
      Equality.areEqual(wanted.asInstanceOf[Function0[_]].apply(),  actual.asInstanceOf[Function0[_]].apply())
    else
      Equality.areEqual(this.wanted, actual)
  }

  override def describeTo(description: Description): Unit = {
    description.appendText(describe(wanted))
  }

  def describe(`object`: AnyRef): String = {
    var text: String = quoting
    text += "" + `object`
    text += quoting
    return text
  }

  private def quoting: String = {
    if (wanted.isInstanceOf[String]) {
      return "\""
    }
    else if (wanted.isInstanceOf[Char]) {
      return "'"
    }
    else {
      return ""
    }
  }

  protected final def getWanted: AnyRef = {
    return wanted
  }

  def equals(o: Any): Boolean = {
    if (o == null || !(this.getClass == o.getClass)) {
      return false
    }
    var other: EqualsFunction0 = o.asInstanceOf[EqualsFunction0]
    this.wanted == null && other.wanted == null || this.wanted != null && (this.wanted == other.wanted)
  }

  override def hashCode: Int = {
    return 1
  }

  def withExtraTypeInfo: SelfDescribing = {
    return new SelfDescribing {
      def describeTo(description: Description): Unit = {
        description.appendText(describe("(" + wanted.getClass.getSimpleName + ") " + wanted))
      }
    }
  }

  def typeMatches(`object`: AnyRef): Boolean = {
    (wanted != null) && (`object` != null) && (`object`.getClass eq wanted.getClass)
  }

  private final var wanted: AnyRef = null
}