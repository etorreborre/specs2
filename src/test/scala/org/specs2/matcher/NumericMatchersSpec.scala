package org.specs2
package matcher

class NumericMatchersSpec extends Specification {  def is =
                                                                                                                        """
The NumericMatchers trait provides matchers to do comparisons with Numeric
types and more generally with Ordered types.
                                                                                                                        """^p^
  "beLessThanOrEqualTo compares any Ordered type with <="                                                               ^
  { 1 must be_<=(2) }                                                                                                   ^
  { 2 must be <=(2) }                                                                                                   ^
  { 1 must beLessThanOrEqualTo(2) }                                                                                     ^
  { 1 must be lessThanOrEqualTo(2) }                                                                                    ^
   "and return a failure if the comparison fails"                                                                       ! e1^
                                                                                                                        p^
  "beLessThan compares any Ordered type with <"                                                                         ^
  { 1 must be_<(2) }                                                                                                    ^
  { 1 must be <(2) }                                                                                                    ^
  { 1 must beLessThan(2) }                                                                                              ^
  { 1 must be lessThan(2) }                                                                                             ^
   "and return a failure if the comparison fails"                                                                       ! e2^
                                                                                                                        p^
  "beGreaterThanOrEqualTo compares any Ordered type with >="                                                            ^
  { 2 must be_>=(1) }                                                                                                   ^
  { 2 must be >=(1) }                                                                                                   ^
  { 2 must beGreaterThanOrEqualTo(1) }                                                                                  ^
  { 2 must be greaterThanOrEqualTo(1) }                                                                                 ^
   "and return a failure if the comparison fails"                                                                       ! e3^
                                                                                                                        p^
  "beGreaterThan compares any Ordered type with >"                                                                      ^
  { 2 must be_>(1) }                                                                                                    ^
  { 2 must be >(1) }                                                                                                    ^
  { 2 must beGreaterThan(1) }                                                                                           ^
  { 2 must be greaterThan(1) }                                                                                          ^
   "and return a failure if the comparison fails"                                                                       ! e4^
                                                                                                                        p^
  "the comparison matchers also work with doubles"                                                                      ^
  { 2.0 must be_>(1.0) }                                                                                                ^
  { 2.0 must be >=(1.0) }                                                                                               ^
                                                                                                                        p^
  "beCloseTo tests if 2 Numerics are close to each other"                                                               ^
  { 1.0 must beCloseTo(1, 0.5) }                                                                                        ^
  { 4 must be ~(5 +/- 2) }                                                                                              ^
  { 2 must not be closeTo(4 +/- 1) }                                                                                    ^
                                                                                                                        end

  def e1 = (2 must be_<=(1)) returns "2 is greater than 1"
  def e2 = (2 must be_<(1))  returns "2 is not less than 1"
  def e3 = (1 must be_>=(2)) returns "1 is less than 2"
  def e4 = (1 must be_>(2))  returns "1 is less than 2"
}