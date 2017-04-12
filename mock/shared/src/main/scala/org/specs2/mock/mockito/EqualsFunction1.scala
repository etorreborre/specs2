package org.mockito.internal.matchers

/*
 * Copyright (c) 2007 Mockito contributors
 * This program is made available under the terms of the MIT License.
 */
/**
 * Ad-hoc implementation of the Equals matcher overriding the matching for Function1 values.
 * It is used to match implicit conversions transparently
 */
case class EqualsFunction1(wanted: scala.Any) extends Equals(wanted) {

  override def matches(actual: Object): Boolean = {
    actual.isInstanceOf[Function1[_,_]]
  }

}
