/*
 * Copyright (c) 2007 Mockito contributors
 * This program is made available under the terms of the MIT License.
 */
package org.mockito.internal.matchers

/**
 * Ad-hoc implementation of the Equals matcher overriding the matching for byname values
 */
case class EqualsFunction0(wanted: scala.Any) extends Equals(wanted) {

  override def matches(actual: scala.Any) = {
    val value = if (actual.isInstanceOf[Function0[_]]) actual.asInstanceOf[Function0[_]].apply() else actual
    Equality.areEqual(super.getWanted, value)
  }

}
