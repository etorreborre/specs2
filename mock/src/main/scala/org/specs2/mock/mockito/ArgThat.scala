package org.specs2
package mock
package mockito


trait ArgThat {

  /** allows to use a specs matcher to match parameters by encapsulating it as a Hamcrest matcher. */
  implicit def argThat[T, U <: T](m: org.specs2.matcher.Matcher[U]): T =
    org.mockito.hamcrest.MockitoHamcrest.argThat(new org.specs2.mock.HamcrestMatcherAdapter(m))

  /** allows to use a hamcrest matchers to match parameters. */
  def anArgThat[T, U <: T](m: org.hamcrest.Matcher[U]): T =
    org.mockito.hamcrest.MockitoHamcrest.argThat(m)

}

object ArgThat extends ArgThat
