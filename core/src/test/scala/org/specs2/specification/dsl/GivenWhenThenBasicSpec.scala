package org.specs2
package specification
package dsl

class GivenWhenThenBasicSpec extends org.specs2.Specification { def is = s2"""
 a class with the GivenWhenThen trait ${step("given")}
 a class with the GivenWhenThen trait ${action("given")}
 a test $ok
"""
}
